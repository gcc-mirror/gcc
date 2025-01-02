/* modules.cc -- D module initialization and termination.
   Copyright (C) 2013-2025 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/declaration.h"
#include "dmd/identifier.h"
#include "dmd/module.h"

#include "tree.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "stor-layout.h"
#include "toplev.h"
#include "target.h"
#include "common/common-target.h"
#include "stringpool.h"

#include "d-tree.h"
#include "d-target.h"


/* D generates module information to inform the runtime library which modules
   need some kind of special handling.  All `static this()', `static ~this()',
   and `unittest' functions for a given module are aggregated into a single
   function - one for each kind - and a pointer to that function is inserted
   into the ModuleInfo instance for that module.

   Module information for a particular module is indicated with an ABI defined
   structure derived from ModuleInfo.  ModuleInfo is a variably sized struct
   with two fixed base fields.  The first field `flags' determines what
   information is packed immediately after the record type.

   Like TypeInfo, the runtime library provides the definitions of the ModuleInfo
   structure, as well as accessors for the variadic fields.  So we only define
   layout compatible POD_structs for ModuleInfo.  */

/* The internally represented ModuleInfo and CompilerDSO types.  */
static tree moduleinfo_type;
static tree compiler_dso_type;
static tree dso_registry_fn;

/* The DSO slot for use by the druntime implementation.  */
static tree dso_slot_node;

/* For registering and deregistering DSOs with druntime, we have one global
   constructor and destructor per object that calls _d_dso_registry with the
   respective DSO record.  To ensure that this is only done once, a
   `dso_initialized' variable is introduced to guard repeated calls.  */
static tree dso_initialized_node;

/* The beginning and end of the `minfo' section.  */
static tree start_minfo_node;
static tree stop_minfo_node;

/* Record information about module initialization, termination,
   unit testing, and thread local storage in the compilation.  */

struct module_info
{
  vec <tree, va_gc> *ctors;
  vec <tree, va_gc> *dtors;
  vec <tree, va_gc> *ctorgates;

  vec <tree, va_gc> *sharedctors;
  vec <tree, va_gc> *shareddtors;
  vec <tree, va_gc> *sharedctorgates;
  vec <tree, va_gc> *standalonectors;

  vec <tree, va_gc> *unitTests;
};

/* These must match the values in libdruntime/object_.d.  */

enum module_info_flags
{
  MIctorstart	    = 0x1,
  MIctordone	    = 0x2,
  MIstandalone	    = 0x4,
  MItlsctor	    = 0x8,
  MItlsdtor	    = 0x10,
  MIctor	    = 0x20,
  MIdtor	    = 0x40,
  MIxgetMembers	    = 0x80,
  MIictor	    = 0x100,
  MIunitTest	    = 0x200,
  MIimportedModules = 0x400,
  MIlocalClasses    = 0x800,
  MIname	    = 0x1000
};

/* The ModuleInfo information structure for the module currently being compiled.
   Assuming that only ever process one at a time.  */

static module_info *current_moduleinfo;

/* When compiling with -fbuilding-libphobos-tests, this contains information
   about the module that gets compiled in only when unittests are enabled.  */

static module_info *current_testing_module;

/* The declaration of the current module being compiled.  */

static Module *current_module_decl;

/* Any inline symbols that were deferred during codegen.  */
vec<Declaration *> *deferred_inline_declarations;

/* Returns an internal function identified by IDENT.  This is used
   by both module initialization and dso handlers.  */

static FuncDeclaration *
get_internal_fn (tree ident, const Visibility &visibility)
{
  Module *mod = current_module_decl;
  const char *name = IDENTIFIER_POINTER (ident);

  if (!mod)
    mod = Module::rootModule;

  if (name[0] == '*')
    {
      tree s = mangle_internal_decl (mod, name + 1, "FZv");
      name = IDENTIFIER_POINTER (s);
    }

  FuncDeclaration *fd = FuncDeclaration::genCfunc (NULL, Type::tvoid,
						   Identifier::idPool (name));
  fd->isGenerated (true);
  fd->loc = Loc (mod->srcfile.toChars (), 1, 0);
  fd->parent = mod;
  fd->visibility = visibility;
  fd->semanticRun = PASS::semantic3done;

  return fd;
}

/* Generate an internal function identified by IDENT.
   The function body to add is in EXPR.  */

static tree
build_internal_fn (tree ident, tree expr)
{
  Visibility visibility;
  visibility.kind = Visibility::private_;
  FuncDeclaration *fd = get_internal_fn (ident, visibility);
  tree decl = get_symbol_decl (fd);

  tree old_context = start_function (fd);
  rest_of_decl_compilation (decl, 1, 0);
  add_stmt (expr);
  finish_function (old_context);

  /* D static ctors, static dtors, unittests, and the ModuleInfo
     chain function are always private.  */
  TREE_PUBLIC (decl) = 0;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;

  return decl;
}

/* Build and emit a function identified by IDENT that increments (in order)
   all variables in GATES, then calls the list of functions in FUNCTIONS.  */

static tree
build_funcs_gates_fn (tree ident, vec <tree, va_gc> *functions,
		      vec <tree, va_gc> *gates)
{
  tree expr_list = NULL_TREE;

  /* Increment gates first.  */
  for (size_t i = 0; i < vec_safe_length (gates); i++)
    {
      tree decl = (*gates)[i];
      tree value = build2 (PLUS_EXPR, TREE_TYPE (decl),
			   decl, integer_one_node);
      tree var_expr = modify_expr (decl, value);
      expr_list = compound_expr (expr_list, var_expr);
    }

  /* Call Functions.  */
  for (size_t i = 0; i < vec_safe_length (functions); i++)
    {
      tree decl = (*functions)[i];
      tree call_expr = build_call_expr (decl, 0);
      expr_list = compound_expr (expr_list, call_expr);
    }

  if (expr_list)
    return build_internal_fn (ident, expr_list);

  return NULL_TREE;
}

/* Return the type for ModuleInfo, create it if it doesn't already exist.  */

static tree
get_moduleinfo_type (void)
{
  if (moduleinfo_type)
    return moduleinfo_type;

  /* Layout of ModuleInfo is:
	uint flags;
	uint index;  */
  tree fields = create_field_decl (d_uint_type, NULL, 1, 1);
  DECL_CHAIN (fields) = create_field_decl (d_uint_type, NULL, 1, 1);

  moduleinfo_type = make_node (RECORD_TYPE);
  finish_builtin_struct (moduleinfo_type, "ModuleInfo", fields, NULL_TREE);

  return moduleinfo_type;
}

/* Get the VAR_DECL of the ModuleInfo for DECL.  If this does not yet exist,
   create it.  The ModuleInfo decl is used to keep track of constructors,
   destructors, unittests, members, classes, and imports for the given module.
   This is used by the D runtime for module initialization and termination.  */

static tree
get_moduleinfo_decl (Module *decl)
{
  if (decl->csym)
    return decl->csym;

  tree ident = mangle_internal_decl (decl, "__ModuleInfo", "Z");
  tree type = get_moduleinfo_type ();

  decl->csym = declare_extern_var (ident, type);
  DECL_LANG_SPECIFIC (decl->csym) = build_lang_decl (NULL);

  DECL_CONTEXT (decl->csym) = build_import_decl (decl);
  /* Not readonly, moduleinit depends on this.  */
  TREE_READONLY (decl->csym) = 0;

  return decl->csym;
}

/* Return the type for CompilerDSOData, create it if it doesn't exist.  */

static tree
get_compiler_dso_type (void)
{
  if (compiler_dso_type)
    return compiler_dso_type;

  /* Layout of CompilerDSOData is:
	size_t version;
	void** slot;
	ModuleInfo** _minfo_beg;
	ModuleInfo** _minfo_end;
	FuncTable* _deh_beg;
	FuncTable* _deh_end;

     Note, finish_builtin_struct() expects these fields in reverse order.  */
  tree fields = create_field_decl (ptr_type_node, NULL, 1, 1);
  tree field = create_field_decl (ptr_type_node, NULL, 1, 1);
  DECL_CHAIN (field) = fields;
  fields = field;

  tree moduleinfo_ptr_ptr_type =
    build_pointer_type (build_pointer_type (get_moduleinfo_type ()));

  field = create_field_decl (moduleinfo_ptr_ptr_type, NULL, 1, 1);
  DECL_CHAIN (field) = fields;
  fields = field;
  field = create_field_decl (moduleinfo_ptr_ptr_type, NULL, 1, 1);
  DECL_CHAIN (field) = fields;
  fields = field;

  field = create_field_decl (build_pointer_type (ptr_type_node), NULL, 1, 1);
  DECL_CHAIN (field) = fields;
  fields = field;

  field = create_field_decl (size_type_node, NULL, 1, 1);
  DECL_CHAIN (field) = fields;
  fields = field;

  compiler_dso_type = make_node (RECORD_TYPE);
  finish_builtin_struct (compiler_dso_type, "CompilerDSOData",
			 fields, NULL_TREE);

  return compiler_dso_type;
}

/* Returns the _d_dso_registry FUNCTION_DECL.  */

static tree
get_dso_registry_fn (void)
{
  if (dso_registry_fn)
    return dso_registry_fn;

  tree dso_type = get_compiler_dso_type ();
  tree fntype = build_function_type_list (void_type_node,
					  build_pointer_type (dso_type),
					  NULL_TREE);
  dso_registry_fn = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
				get_identifier ("_d_dso_registry"), fntype);
  TREE_PUBLIC (dso_registry_fn) = 1;
  DECL_EXTERNAL (dso_registry_fn) = 1;

  return dso_registry_fn;
}

/* Depending on CTOR_P, builds and emits eiter a constructor or destructor
   calling _d_dso_registry if `dso_initialized' is `false' in a constructor
   or `true' in a destructor.  */

static tree
build_dso_cdtor_fn (bool ctor_p)
{
  const char *name = ctor_p ? GDC_PREFIX ("dso_ctor") : GDC_PREFIX ("dso_dtor");
  tree condition = ctor_p ? d_bool_true_node : d_bool_false_node;

  /* Declaration of dso_ctor/dso_dtor is:

     extern(C) void dso_{c,d}tor (void)
     {
	if (dso_initialized != condition)
	{
	    dso_initialized = condition;
	    CompilerDSOData dso = {1, &dsoSlot, &__start_minfo, &__stop_minfo};
	    _d_dso_registry (&dso);
	}
    }
   */
  Visibility visibility;
  visibility.kind = Visibility::public_;
  FuncDeclaration *fd = get_internal_fn (get_identifier (name), visibility);
  tree decl = get_symbol_decl (fd);

  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;

  /* Start laying out the body.  */
  tree old_context = start_function (fd);
  rest_of_decl_compilation (decl, 1, 0);

  /* if (dso_initialized != condition).  */
  tree if_cond = build_boolop (NE_EXPR, dso_initialized_node, condition);

  /* dso_initialized = condition;  */
  tree expr_list = modify_expr (dso_initialized_node, condition);

  /* CompilerDSOData dso = {1, &dsoSlot, &__start_minfo, &__stop_minfo};  */
  tree dso_type = get_compiler_dso_type ();
  tree dso = build_local_temp (dso_type);

  vec <constructor_elt, va_gc> *ve = NULL;
  CONSTRUCTOR_APPEND_ELT (ve, NULL_TREE, build_integer_cst (1, size_type_node));
  CONSTRUCTOR_APPEND_ELT (ve, NULL_TREE, build_address (dso_slot_node));
  CONSTRUCTOR_APPEND_ELT (ve, NULL_TREE, build_address (start_minfo_node));
  CONSTRUCTOR_APPEND_ELT (ve, NULL_TREE, build_address (stop_minfo_node));

  tree assign_expr = modify_expr (dso, build_struct_literal (dso_type, ve));
  expr_list = compound_expr (expr_list, assign_expr);

  /* _d_dso_registry (&dso);  */
  tree call_expr = build_call_expr (get_dso_registry_fn (), 1,
				    build_address (dso));
  expr_list = compound_expr (expr_list, call_expr);

  add_stmt (build_vcondition (if_cond, expr_list, void_node));
  finish_function (old_context);

  return decl;
}

/* Build a variable used in the dso_registry code identified by NAME,
   and data type TYPE.  The variable always has VISIBILITY_HIDDEN and
   TREE_PUBLIC flags set.  */

static tree
build_dso_registry_var (const char * name, tree type)
{
  tree var = declare_extern_var (get_identifier (name), type);
  DECL_VISIBILITY (var) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (var) = 1;
  return var;
}

/* Place a reference to the ModuleInfo symbol MINFO for DECL into the
   `minfo' section.  Then create the global ctors/dtors to call the
   _d_dso_registry function if necessary.  */

static void
register_moduleinfo (Module *decl, tree minfo)
{
  /* No defined minfo section for target.  */
  if (targetdm.d_minfo_section == NULL)
    return;

  if (!targetm_common.have_named_sections)
    sorry ("%<-fmoduleinfo%> is not supported on this target");

  /* Build the ModuleInfo reference, this is done once for every Module.  */
  tree ident = mangle_internal_decl (decl, "__moduleRef", "Z");
  tree mref = declare_extern_var (ident, ptr_type_node);

  /* Build the initializer and emit.  Do not start section with a `.' character
     so that the linker will provide a __start_ and __stop_ symbol to indicate
     the start and end address of the section respectively.
     https://sourceware.org/binutils/docs-2.26/ld/Orphan-Sections.html.  */
  DECL_INITIAL (mref) = build_address (minfo);
  DECL_EXTERNAL (mref) = 0;
  DECL_PRESERVE_P (mref) = 1;

  set_decl_section_name (mref, targetdm.d_minfo_section);
  symtab_node::get (mref)->implicit_section = true;
  d_pushdecl (mref);
  rest_of_decl_compilation (mref, 1, 0);

  /* Only for the first D module being emitted do we need to generate a static
     constructor and destructor for.  These are only required once per shared
     library, so it's safe to emit them only once per object file.  */
  static bool first_module = true;
  if (!first_module)
    return;

  start_minfo_node = build_dso_registry_var (targetdm.d_minfo_section_start,
					     ptr_type_node);
  rest_of_decl_compilation (start_minfo_node, 1, 0);

  stop_minfo_node = build_dso_registry_var (targetdm.d_minfo_section_end,
					    ptr_type_node);
  rest_of_decl_compilation (stop_minfo_node, 1, 0);

  /* Declare dso_slot and dso_initialized.  */
  dso_slot_node = build_dso_registry_var (GDC_PREFIX ("dso_slot"),
					  ptr_type_node);
  d_finish_decl (dso_slot_node);

  dso_initialized_node = build_dso_registry_var (GDC_PREFIX ("dso_initialized"),
						 d_bool_type);
  d_finish_decl (dso_initialized_node);

  /* Declare dso_ctor() and dso_dtor().  */
  tree dso_ctor = build_dso_cdtor_fn (true);
  DECL_STATIC_CONSTRUCTOR (dso_ctor) = 1;
  decl_init_priority_insert (dso_ctor, DEFAULT_INIT_PRIORITY);

  tree dso_dtor = build_dso_cdtor_fn (false);
  DECL_STATIC_DESTRUCTOR (dso_dtor) = 1;
  decl_fini_priority_insert (dso_dtor, DEFAULT_INIT_PRIORITY);

  first_module = false;
}

/* Convenience function for layout_moduleinfo_fields.  Adds a field of TYPE to
   the moduleinfo record at OFFSET, incrementing the offset to the next field
   position.  No alignment is taken into account, all fields are packed.  */

static void
layout_moduleinfo_field (tree type, tree rec_type, HOST_WIDE_INT &offset)
{
  tree field = create_field_decl (type, NULL, 1, 1);
  insert_aggregate_field (rec_type, field, offset);
  offset += int_size_in_bytes (type);
}

/* Layout fields that immediately come after the moduleinfo TYPE for DECL.
   Data relating to the module is packed into the type on an as-needed
   basis, this is done to keep its size to a minimum.  */

static tree
layout_moduleinfo_fields (Module *decl, tree type)
{
  HOST_WIDE_INT offset = int_size_in_bytes (type);
  type = copy_aggregate_type (type);

  /* First fields added are all the function pointers.  */
  if (decl->sctor)
    layout_moduleinfo_field (ptr_type_node, type, offset);

  if (decl->sdtor)
    layout_moduleinfo_field (ptr_type_node, type, offset);

  if (decl->ssharedctor)
    layout_moduleinfo_field (ptr_type_node, type, offset);

  if (decl->sshareddtor)
    layout_moduleinfo_field (ptr_type_node, type, offset);

  if (dmd::findGetMembers (decl))
    layout_moduleinfo_field (ptr_type_node, type, offset);

  if (decl->sictor)
    layout_moduleinfo_field (ptr_type_node, type, offset);

  if (decl->stest)
    layout_moduleinfo_field (ptr_type_node, type, offset);

  /* Array of module imports is laid out as a length field, followed by
     a static array of ModuleInfo pointers.  */
  size_t aimports_dim = decl->aimports.length;
  for (size_t i = 0; i < decl->aimports.length; i++)
    {
      Module *mi = decl->aimports[i];
      if (!mi->needmoduleinfo)
	aimports_dim--;
    }

  if (aimports_dim)
    {
      layout_moduleinfo_field (size_type_node, type, offset);
      layout_moduleinfo_field (make_array_type (Type::tvoidptr, aimports_dim),
			       type, offset);
    }

  /* Array of local ClassInfo decls are laid out in the same way.  */
  ClassDeclarations aclasses;
  dmd::getLocalClasses (decl, aclasses);

  if (aclasses.length)
    {
      layout_moduleinfo_field (size_type_node, type, offset);
      layout_moduleinfo_field (make_array_type (Type::tvoidptr,
						aclasses.length),
			       type, offset);
    }

  /* Lastly, the name of the module is a static char array.  */
  size_t namelen = strlen (decl->toPrettyChars ()) + 1;
  layout_moduleinfo_field (make_array_type (Type::tchar, namelen),
			   type, offset);

  size_t alignsize = MAX (TYPE_ALIGN_UNIT (type),
			  TYPE_ALIGN_UNIT (ptr_type_node));
  finish_aggregate_type (offset, alignsize, type);

  return type;
}

/* Output the ModuleInfo for module DECL and register it with druntime.  */

static void
layout_moduleinfo (Module *decl)
{
  ClassDeclarations aclasses;
  FuncDeclaration *sgetmembers;

  dmd::getLocalClasses (decl, aclasses);

  size_t aimports_dim = decl->aimports.length;
  for (size_t i = 0; i < decl->aimports.length; i++)
    {
      Module *mi = decl->aimports[i];
      if (!mi->needmoduleinfo)
	aimports_dim--;
    }

  sgetmembers = dmd::findGetMembers (decl);

  size_t flags = 0;
  if (decl->sctor)
    flags |= MItlsctor;
  if (decl->sdtor)
    flags |= MItlsdtor;
  if (decl->ssharedctor)
    flags |= MIctor;
  if (decl->sshareddtor)
    flags |= MIdtor;
  if (sgetmembers)
    flags |= MIxgetMembers;
  if (decl->sictor)
    flags |= MIictor;
  if (decl->stest)
    flags |= MIunitTest;
  if (aimports_dim)
    flags |= MIimportedModules;
  if (aclasses.length)
    flags |= MIlocalClasses;
  if (!decl->needmoduleinfo)
    flags |= MIstandalone;

  flags |= MIname;

  tree minfo = get_moduleinfo_decl (decl);
  tree type = layout_moduleinfo_fields (decl, TREE_TYPE (minfo));

  /* Put out the two named fields in a ModuleInfo decl:
	uint flags;
	uint index;  */
  vec <constructor_elt, va_gc> *minit = NULL;

  CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			  build_integer_cst (flags, d_uint_type));

  CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			  build_integer_cst (0, d_uint_type));

  /* Order of appearance, depending on flags:
	void function() tlsctor;
	void function() tlsdtor;
	void* function() xgetMembers;
	void function() ctor;
	void function() dtor;
	void function() ictor;
	void function() unitTest;
	ModuleInfo*[] importedModules;
	TypeInfo_Class[] localClasses;
	char[N] name;
   */
  if (flags & MItlsctor)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, build_address (decl->sctor));

  if (flags & MItlsdtor)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, build_address (decl->sdtor));

  if (flags & MIctor)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			    build_address (decl->ssharedctor));

  if (flags & MIdtor)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			    build_address (decl->sshareddtor));

  if (flags & MIxgetMembers)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			    build_address (get_symbol_decl (sgetmembers)));

  if (flags & MIictor)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, build_address (decl->sictor));

  if (flags & MIunitTest)
    CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, build_address (decl->stest));

  if (flags & MIimportedModules)
    {
      vec <constructor_elt, va_gc> *elms = NULL;
      tree satype = make_array_type (Type::tvoidptr, aimports_dim);
      size_t idx = 0;

      for (size_t i = 0; i < decl->aimports.length; i++)
	{
	  Module *mi = decl->aimports[i];
	  if (mi->needmoduleinfo)
	    {
	      CONSTRUCTOR_APPEND_ELT (elms, size_int (idx),
				      build_address (get_moduleinfo_decl (mi)));
	      idx++;
	    }
	}

      CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, size_int (aimports_dim));
      CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			      build_constructor (satype, elms));
    }

  if (flags & MIlocalClasses)
    {
      vec <constructor_elt, va_gc> *elms = NULL;
      tree satype = make_array_type (Type::tvoidptr, aclasses.length);

      for (size_t i = 0; i < aclasses.length; i++)
	{
	  ClassDeclaration *cd = aclasses[i];
	  CONSTRUCTOR_APPEND_ELT (elms, size_int (i),
				  build_address (get_classinfo_decl (cd)));
	}

      CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, size_int (aclasses.length));
      CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE,
			      build_constructor (satype, elms));
    }

  if (flags & MIname)
    {
      /* Put out module name as a 0-terminated C-string, to save bytes.  */
      const char *name = decl->toPrettyChars ();
      size_t namelen = strlen (name) + 1;
      tree strtree = build_string (namelen, name);
      TREE_TYPE (strtree) = make_array_type (Type::tchar, namelen);
      CONSTRUCTOR_APPEND_ELT (minit, NULL_TREE, strtree);
    }

  TREE_TYPE (minfo) = type;
  DECL_INITIAL (minfo) = build_struct_literal (type, minit);
  d_finish_decl (minfo);

  /* Register the module against druntime.  */
  register_moduleinfo (decl, minfo);
}

/* Send the Module AST class DECL to GCC back-end.  */

void
build_module_tree (Module *decl)
{
  /* There may be more than one module per object file, but should only
     ever compile them one at a time.  */
  assert (!current_moduleinfo && !current_module_decl);

  module_info mi = module_info ();
  module_info mitest = module_info ();

  current_moduleinfo = &mi;
  current_testing_module = &mitest;
  current_module_decl = decl;

  vec<Declaration *> deferred_decls = vNULL;
  deferred_inline_declarations = &deferred_decls;

  /* Layout module members.  */
  if (decl->members)
    {
      for (size_t i = 0; i < decl->members->length; i++)
	{
	  Dsymbol *s = (*decl->members)[i];
	  build_decl_tree (s);
	}
    }

  /* For libphobos-internal use only.  Generate a separate module info symbol
     that references all compiled in unittests, this allows compiling library
     modules and linking to libphobos without having run-time conflicts because
     of two ModuleInfo records with the same name being present in two DSOs.  */
  if (flag_building_libphobos_tests)
    {
      /* Associate the module info symbol with a mock module.  */
      const char *name = concat (GDC_PREFIX ("modtest__"),
				 decl->ident->toChars (), NULL);
      Module *tm = Module::create (decl->arg.ptr, Identifier::idPool (name),
				   0, 0);
      Dsymbols members;

      /* Setting parent puts module in the same package as the current, to
	 avoid any symbol conflicts.  */
      tm->parent = decl->parent;
      tm->needmoduleinfo = decl->needmoduleinfo;
      tm->members = &members;
      /* Register the current module as being imported by the mock module.
	 This informs run-time that there is a dependency between the two.  */
      tm->aimports.push (decl);

      if (mitest.ctors || mitest.ctorgates)
	tm->sctor = build_funcs_gates_fn (get_identifier ("*__modtestctor"),
					  mitest.ctors, mitest.ctorgates);

      if (mitest.dtors)
	tm->sdtor = build_funcs_gates_fn (get_identifier ("*__modtestdtor"),
					  mitest.dtors, NULL);

      if (mi.standalonectors)
	tm->sictor
	  = build_funcs_gates_fn (get_identifier ("*__modtestsharedictor"),
				  mi.standalonectors, NULL);

      if (mitest.sharedctors || mitest.sharedctorgates)
	tm->ssharedctor
	  = build_funcs_gates_fn (get_identifier ("*__modtestsharedctor"),
				  mitest.sharedctors, mitest.sharedctorgates);

      if (mitest.shareddtors)
	tm->sshareddtor
	  = build_funcs_gates_fn (get_identifier ("*__modtestshareddtor"),
				  mitest.shareddtors, NULL);

      if (mi.unitTests)
	tm->stest = build_funcs_gates_fn (get_identifier ("*__modtest"),
					  mi.unitTests, NULL);

      mi.unitTests = NULL;
      layout_moduleinfo (tm);
    }

  /* Default behavior is to always generate module info because of templates.
     Can be switched off for not compiling against runtime library.  */
  if (global.params.useModuleInfo && Module::moduleinfo != NULL)
    {
      if (mi.ctors || mi.ctorgates)
	decl->sctor = build_funcs_gates_fn (get_identifier ("*__modctor"),
					    mi.ctors, mi.ctorgates);

      if (mi.dtors)
	decl->sdtor = build_funcs_gates_fn (get_identifier ("*__moddtor"),
					    mi.dtors, NULL);

      if (mi.standalonectors)
	decl->sictor
	  = build_funcs_gates_fn (get_identifier ("*__modsharedictor"),
				  mi.standalonectors, NULL);

      if (mi.sharedctors || mi.sharedctorgates)
	decl->ssharedctor
	  = build_funcs_gates_fn (get_identifier ("*__modsharedctor"),
				  mi.sharedctors, mi.sharedctorgates);

      if (mi.shareddtors)
	decl->sshareddtor
	  = build_funcs_gates_fn (get_identifier ("*__modshareddtor"),
				  mi.shareddtors, NULL);

      if (mi.unitTests)
	decl->stest = build_funcs_gates_fn (get_identifier ("*__modtest"),
					    mi.unitTests, NULL);

      layout_moduleinfo (decl);
    }

  /* Process all deferred functions after finishing module.  */
  for (size_t i = 0; i < deferred_decls.length (); ++i)
    build_decl_tree (deferred_decls[i]);

  current_moduleinfo = NULL;
  current_testing_module = NULL;
  current_module_decl = NULL;
  deferred_inline_declarations = NULL;
}

/* Returns the current function or module context for the purpose
   of imported_module_or_decl.  */

tree
d_module_context (void)
{
  if (cfun != NULL)
    return current_function_decl;

  gcc_assert (current_module_decl != NULL);
  return build_import_decl (current_module_decl);
}

/* Maybe record declaration D against our module information structure.  */

void
register_module_decl (Declaration *d)
{
  FuncDeclaration *fd = d->isFuncDeclaration ();
  if (fd != NULL)
    {
      tree decl = get_symbol_decl (fd);

      /* Any module constructors or destructors that are only present when
	 compiling in unittests are kept track of separately so they are
	 not omitted when compiling with -fbuilding-libphobos-tests.  */
      module_info *minfo;
      if (flag_building_libphobos_tests && !fd->isUnitTestDeclaration ()
	  && DECL_IN_UNITTEST_CONDITION_P (decl))
	minfo = current_testing_module;
      else
	minfo = current_moduleinfo;

      gcc_assert (minfo != NULL);

      /* If a static constructor, push into the current ModuleInfo.
	 Checks for `shared' first because it derives from the non-shared
	 constructor type in the front-end.  */
      if (SharedStaticCtorDeclaration *sctor
	  = fd->isSharedStaticCtorDeclaration ())
	{
	  /* The `shared' static constructor was marked `@standalone'.  */
	  if (sctor->standalone)
	    vec_safe_push (minfo->standalonectors, decl);
	  else
	    vec_safe_push (minfo->sharedctors, decl);
	}
      else if (fd->isStaticCtorDeclaration ())
	vec_safe_push (minfo->ctors, decl);

      /* If a static destructor, do same as with constructors, but also
	 increment the destructor's vgate at construction time.  */
      if (fd->isSharedStaticDtorDeclaration ())
	{
	  VarDeclaration *vgate = ((SharedStaticDtorDeclaration *) fd)->vgate;
	  if (vgate != NULL)
	    {
	      tree gate = get_symbol_decl (vgate);
	      vec_safe_push (minfo->sharedctorgates, gate);
	    }
	  vec_safe_insert (minfo->shareddtors, 0, decl);
	}
      else if (fd->isStaticDtorDeclaration ())
	{
	  VarDeclaration *vgate = ((StaticDtorDeclaration *) fd)->vgate;
	  if (vgate != NULL)
	    {
	      tree gate = get_symbol_decl (vgate);
	      vec_safe_push (minfo->ctorgates, gate);
	    }
	  vec_safe_insert (minfo->dtors, 0, decl);
	}

      /* If a unittest function.  */
      if (fd->isUnitTestDeclaration ())
	vec_safe_push (minfo->unitTests, decl);
    }
}

/* Add DECL as a declaration to emit at the end of the current module.  */

void
d_defer_declaration (Declaration *decl)
{
  gcc_assert (deferred_inline_declarations != NULL);
  deferred_inline_declarations->safe_push (decl);
}

/* Wrapup all global declarations and start the final compilation.  */

void
d_finish_compilation (tree *vec, int len)
{
  /* Complete all generated thunks.  */
  symtab->process_same_body_aliases ();

  /* Process all file scopes in this compilation, and the external_scope,
     through wrapup_global_declarations.  */
  for (int i = 0; i < len; i++)
    {
      tree decl = vec[i];
      wrapup_global_declarations (&decl, 1);
    }
}
