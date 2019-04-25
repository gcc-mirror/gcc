/* d-builtins.cc -- GCC builtins support for D.
   Copyright (C) 2006-2019 Free Software Foundation, Inc.

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

#include "dmd/attrib.h"
#include "dmd/aggregate.h"
#include "dmd/cond.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/module.h"
#include "dmd/mtype.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "target.h"
#include "common/common-target.h"
#include "stringpool.h"
#include "stor-layout.h"

#include "d-tree.h"
#include "d-target.h"


static GTY(()) vec<tree, va_gc> *gcc_builtins_functions = NULL;
static GTY(()) vec<tree, va_gc> *gcc_builtins_libfuncs = NULL;
static GTY(()) vec<tree, va_gc> *gcc_builtins_types = NULL;

/* Record built-in types and their associated decls for re-use when
   generating the `gcc.builtins' module.  */

struct builtin_data
{
  Type *dtype;
  tree ctype;
  Dsymbol *dsym;

  builtin_data (Type *t, tree c, Dsymbol *d = NULL)
    : dtype(t), ctype(c), dsym(d)
  { }
};

static vec<builtin_data> builtin_converted_decls;

/* Build D frontend type from tree TYPE type given.  This will set the
   back-end type symbol directly for complex types to save build_ctype()
   the work.  For other types, it is not useful or will cause errors, such
   as casting from `C char' to `D char', which also means that `char *`
   needs to be specially handled.  */

static Type *
build_frontend_type (tree type)
{
  Type *dtype;
  MOD mod = 0;

  if (TYPE_READONLY (type))
    mod |= MODconst;
  if (TYPE_VOLATILE (type))
    mod |= MODshared;

  /* If we've seen the type before, re-use the converted decl.  */
  for (size_t i = 0; i < builtin_converted_decls.length (); ++i)
    {
      tree t = builtin_converted_decls[i].ctype;
      if (TYPE_MAIN_VARIANT (t) == TYPE_MAIN_VARIANT (type))
	return builtin_converted_decls[i].dtype;
    }

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  /* Check for char * first.  Needs to be done for chars/string.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (type)) == char_type_node)
	    return Type::tchar->addMod (dtype->mod)->pointerTo ()->addMod (mod);

	  if (dtype->ty == Tfunction)
	    return (TypePointer::create (dtype))->addMod (mod);

	  return dtype->pointerTo ()->addMod (mod);
	}
      break;

    case REFERENCE_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  /* Want to assign ctype directly so that the REFERENCE_TYPE code
	     can be turned into as an `inout' argument.  Can't use pointerTo(),
	     because the returned Type is shared.  */
	  dtype = (TypePointer::create (dtype))->addMod (mod);
	  dtype->ctype = type;
	  builtin_converted_decls.safe_push (builtin_data (dtype, type));
	  return dtype;
	}
      break;

    case BOOLEAN_TYPE:
      /* Should be no need for size checking.  */
      return Type::tbool->addMod (mod);

    case INTEGER_TYPE:
    {
      unsigned size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
      bool unsignedp = TYPE_UNSIGNED (type);

      /* For now, skip support for cent/ucent until the frontend
	 has better support for handling it.  */
      for (size_t i = Tint8; i <= Tuns64; i++)
	{
	  dtype = Type::basic[i];

	  /* Search for type matching size and signedness.  */
	  if (unsignedp != dtype->isunsigned ()
	      || size != dtype->size ())
	    continue;

	  return dtype->addMod (mod);
	}
      break;
    }

    case REAL_TYPE:
    {
      unsigned size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));

      for (size_t i = Tfloat32; i <= Tfloat80; i++)
	{
	  dtype = Type::basic[i];

	  /* Search for type matching size.  */
	  if (dtype->size () != size)
	    continue;

	  return dtype->addMod (mod);
	}
      break;
    }

    case COMPLEX_TYPE:
    {
      unsigned size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
      for (size_t i = Tcomplex32; i <= Tcomplex80; i++)
	{
	  dtype = Type::basic[i];

	  /* Search for type matching size.  */
	  if (dtype->size () != size)
	    continue;

	  return dtype->addMod (mod);
	}
      break;
    }

    case VOID_TYPE:
      return Type::tvoid->addMod (mod);

    case ARRAY_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  tree index = TYPE_DOMAIN (type);
	  tree ub = TYPE_MAX_VALUE (index);
	  tree lb = TYPE_MIN_VALUE (index);

	  tree length = fold_build2 (MINUS_EXPR, TREE_TYPE (lb), ub, lb);
	  length = size_binop (PLUS_EXPR, size_one_node,
			       convert (sizetype, length));

	  dtype = dtype->sarrayOf (TREE_INT_CST_LOW (length))->addMod (mod);
	  builtin_converted_decls.safe_push (builtin_data (dtype, type));
	  return dtype;
	}
      break;

    case VECTOR_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (type);
	  dtype = dtype->sarrayOf (nunits.to_constant ())->addMod (mod);

	  if (dtype->nextOf ()->isTypeBasic () == NULL)
	    break;

	  dtype = (TypeVector::create (Loc (), dtype))->addMod (mod);
	  builtin_converted_decls.safe_push (builtin_data (dtype, type));
	  return dtype;
	}
      break;

    case RECORD_TYPE:
      if (TYPE_NAME (type))
	{
	  tree structname = DECL_NAME (TYPE_NAME (type));
	  Identifier *ident
	    = Identifier::idPool (IDENTIFIER_POINTER (structname));

	  /* Neither the `object' and `gcc.builtins' modules will not exist when
	     this is called.  Use a stub 'object' module parent in the meantime.
	     If `gcc.builtins' is later imported, the parent will be overridden
	     with the correct module symbol.  */
	  static Identifier *object = Identifier::idPool ("object");
	  static Module *stubmod = Module::create ("object.d", object, 0, 0);

	  StructDeclaration *sdecl = StructDeclaration::create (Loc (), ident,
								false);
	  sdecl->parent = stubmod;
	  sdecl->structsize = int_size_in_bytes (type);
	  sdecl->alignsize = TYPE_ALIGN_UNIT (type);
	  sdecl->alignment = STRUCTALIGN_DEFAULT;
	  sdecl->sizeok = SIZEOKdone;
	  sdecl->type = (TypeStruct::create (sdecl))->addMod (mod);
	  sdecl->type->ctype = type;
	  sdecl->type->merge2 ();

	  /* Does not seem necessary to convert fields, but the members field
	     must be non-null for the above size setting to stick.  */
	  sdecl->members = new Dsymbols;
	  dtype = sdecl->type;
	  builtin_converted_decls.safe_push (builtin_data (dtype, type, sdecl));
	  return dtype;
	}
      break;

    case FUNCTION_TYPE:
      dtype = build_frontend_type (TREE_TYPE (type));
      if (dtype)
	{
	  tree parms = TYPE_ARG_TYPES (type);
	  int varargs_p = 1;

	  Parameters *args = new Parameters;
	  args->reserve (list_length (parms));

	  /* Attempt to convert all parameter types.  */
	  for (tree parm = parms; parm != NULL_TREE; parm = TREE_CHAIN (parm))
	    {
	      tree argtype = TREE_VALUE (parm);
	      if (argtype == void_type_node)
		{
		  varargs_p = 0;
		  break;
		}

	      StorageClass sc = STCundefined;
	      if (TREE_CODE (argtype) == REFERENCE_TYPE)
		{
		  argtype = TREE_TYPE (argtype);
		  sc |= STCref;
		}

	      Type *targ = build_frontend_type (argtype);
	      if (!targ)
		{
		  delete args;
		  return NULL;
		}

	      args->push (Parameter::create (sc, targ, NULL, NULL));
	    }

	  /* GCC generic and placeholder built-ins are marked as variadic, yet
	     have no named parameters, and so can't be represented in D.  */
	  if (args->dim != 0 || !varargs_p)
	    {
	      dtype = TypeFunction::create (args, dtype, varargs_p, LINKc);
	      return dtype->addMod (mod);
	    }
	}
      break;

    default:
      break;
    }

  return NULL;
}

/* Attempt to convert GCC evaluated CST to a D Frontend Expression.
   This is used for getting the CTFE value out of a const-folded builtin,
   returns NULL if it cannot convert CST.  */

Expression *
d_eval_constant_expression (tree cst)
{
  STRIP_TYPE_NOPS (cst);
  Type *type = build_frontend_type (TREE_TYPE (cst));

  if (type)
    {
      /* Convert our GCC CST tree into a D Expression.  This seems like we are
	 trying too hard, as these will only be converted back to a tree again
	 later in the codegen pass, but satisfies the need to have GCC built-ins
	 CTFE-able in the frontend.  */
      tree_code code = TREE_CODE (cst);
      if (code == COMPLEX_CST)
	{
	  real_value re = TREE_REAL_CST (TREE_REALPART (cst));
	  real_value im = TREE_REAL_CST (TREE_IMAGPART (cst));
	  complex_t value = complex_t (ldouble (re), ldouble (im));
	  return ComplexExp::create (Loc (), value, type);
	}
      else if (code == INTEGER_CST)
	{
	  dinteger_t value = TREE_INT_CST_LOW (cst);
	  return IntegerExp::create (Loc (), value, type);
	}
      else if (code == REAL_CST)
	{
	  real_value value = TREE_REAL_CST (cst);
	  return RealExp::create (Loc (), ldouble (value), type);
	}
      else if (code == STRING_CST)
	{
	  const void *string = TREE_STRING_POINTER (cst);
	  size_t len = TREE_STRING_LENGTH (cst);
	  return StringExp::create (Loc (), CONST_CAST (void *, string), len);
	}
      else if (code == VECTOR_CST)
	{
	  dinteger_t nunits = VECTOR_CST_NELTS (cst).to_constant ();
	  Expressions *elements = new Expressions;
	  elements->setDim (nunits);

	  for (size_t i = 0; i < nunits; i++)
	    {
	      Expression *elem
		= d_eval_constant_expression (VECTOR_CST_ELT (cst, i));
	      if (elem == NULL)
		return NULL;

	      (*elements)[i] = elem;
	    }

	  Expression *e = ArrayLiteralExp::create (Loc (), elements);
	  e->type = ((TypeVector *) type)->basetype;

	  return VectorExp::create (Loc (), e, type);
	}
    }

  return NULL;
}

/* Callback for TARGET_D_CPU_VERSIONS and TARGET_D_OS_VERSIONS.
   Adds IDENT to the list of predefined version identifiers.  */

void
d_add_builtin_version (const char* ident)
{
  /* For now, we need to tell the D frontend what platform is being targeted.
     This should be removed once the frontend has been fixed.  */
  if (strcmp (ident, "linux") == 0)
    global.params.isLinux = true;
  else if (strcmp (ident, "OSX") == 0)
    global.params.isOSX = true;
  else if (strcmp (ident, "Windows") == 0)
    global.params.isWindows = true;
  else if (strcmp (ident, "FreeBSD") == 0)
    global.params.isFreeBSD = true;
  else if (strcmp (ident, "OpenBSD") == 0)
    global.params.isOpenBSD = true;
  else if (strcmp (ident, "Solaris") == 0)
    global.params.isSolaris = true;
  /* The is64bit field only refers to x86_64 target.  */
  else if (strcmp (ident, "X86_64") == 0)
    global.params.is64bit = true;
  /* No other fields are required to be set for the frontend.  */

  VersionCondition::addPredefinedGlobalIdent (ident);
}

/* Initialize the list of all the predefined version identifiers.  */

void
d_init_versions (void)
{
  VersionCondition::addPredefinedGlobalIdent ("GNU");
  VersionCondition::addPredefinedGlobalIdent ("D_Version2");

  if (BYTES_BIG_ENDIAN)
    VersionCondition::addPredefinedGlobalIdent ("BigEndian");
  else
    VersionCondition::addPredefinedGlobalIdent ("LittleEndian");

  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    VersionCondition::addPredefinedGlobalIdent ("GNU_SjLj_Exceptions");
  else if (targetm_common.except_unwind_info (&global_options) == UI_SEH)
    VersionCondition::addPredefinedGlobalIdent ("GNU_SEH_Exceptions");
  else if (targetm_common.except_unwind_info (&global_options) == UI_DWARF2)
    VersionCondition::addPredefinedGlobalIdent ("GNU_DWARF2_Exceptions");

  if (!targetm.have_tls)
    VersionCondition::addPredefinedGlobalIdent ("GNU_EMUTLS");

  if (STACK_GROWS_DOWNWARD)
    VersionCondition::addPredefinedGlobalIdent ("GNU_StackGrowsDown");

  /* Should define this anyway to set us apart from the competition.  */
  VersionCondition::addPredefinedGlobalIdent ("GNU_InlineAsm");

  /* LP64 only means 64bit pointers in D.  */
  if (global.params.isLP64)
    VersionCondition::addPredefinedGlobalIdent ("D_LP64");

  /* Setting `global.params.cov' forces module info generation which is
     not needed for the GCC coverage implementation.  Instead, just
     test flag_test_coverage while leaving `global.params.cov' unset.  */
  if (flag_test_coverage)
    VersionCondition::addPredefinedGlobalIdent ("D_Coverage");
  if (flag_pic)
    VersionCondition::addPredefinedGlobalIdent ("D_PIC");

  if (global.params.doDocComments)
    VersionCondition::addPredefinedGlobalIdent ("D_Ddoc");

  if (global.params.useUnitTests)
    VersionCondition::addPredefinedGlobalIdent ("unittest");

  if (global.params.useAssert)
    VersionCondition::addPredefinedGlobalIdent ("assert");

  if (global.params.useArrayBounds == BOUNDSCHECKoff)
    VersionCondition::addPredefinedGlobalIdent ("D_NoBoundsChecks");

  if (global.params.betterC)
    VersionCondition::addPredefinedGlobalIdent ("D_BetterC");
  else
    {
      VersionCondition::addPredefinedGlobalIdent ("D_ModuleInfo");
      VersionCondition::addPredefinedGlobalIdent ("D_Exceptions");
      VersionCondition::addPredefinedGlobalIdent ("D_TypeInfo");
    }

  VersionCondition::addPredefinedGlobalIdent ("all");

  /* Emit all target-specific version identifiers.  */
  targetdm.d_cpu_versions ();
  targetdm.d_os_versions ();

  VersionCondition::addPredefinedGlobalIdent ("CppRuntime_Gcc");
}

/* A helper for d_build_builtins_module.  Return a new ALIAS for TYPE.
   Analogous to `alias ALIAS = TYPE' in D code.  */

static AliasDeclaration *
build_alias_declaration (const char *alias, Type *type)
{
  return AliasDeclaration::create (Loc (), Identifier::idPool (alias), type);
}

/* A helper function for Target::loadModule.  Generates all code for the
   `gcc.builtins' module, whose frontend symbol should be M.  */

void
d_build_builtins_module (Module *m)
{
  Dsymbols *members = new Dsymbols;
  tree decl;

  for (size_t i = 0; vec_safe_iterate (gcc_builtins_functions, i, &decl); ++i)
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
      TypeFunction *tf
	= (TypeFunction *) build_frontend_type (TREE_TYPE (decl));

      /* Cannot create built-in function type for DECL.  */
      if (!tf)
	continue;

      /* A few notes on D2 attributes applied to builtin functions:
	 - It is assumed that built-ins solely provided by the compiler are
	   considered @safe and pure.
	 - Built-ins that correspond to `extern(C)' functions in the standard
	   library that have `__attribute__(nothrow)' are considered `@trusted'.
	 - The purity of a built-in can vary depending on compiler flags set
	   upon initialization, or by the `-foptions' passed, such as
	   flag_unsafe_math_optimizations.
	 - Built-ins never use the GC or raise a D exception, and so are always
	   marked as `nothrow' and `@nogc'.  */
      tf->purity = DECL_PURE_P (decl) ? PUREstrong
	: TREE_READONLY (decl) ? PUREconst
	: DECL_IS_NOVOPS (decl) ? PUREweak
	: !DECL_ASSEMBLER_NAME_SET_P (decl) ? PUREweak
	: PUREimpure;
      tf->trust = !DECL_ASSEMBLER_NAME_SET_P (decl) ? TRUSTsafe
	: TREE_NOTHROW (decl) ? TRUSTtrusted
	: TRUSTsystem;
      tf->isnothrow = true;
      tf->isnogc = true;

      FuncDeclaration *func
	= FuncDeclaration::create (Loc (), Loc (),
				   Identifier::idPool (name),
				   STCextern, tf);
      DECL_LANG_SPECIFIC (decl) = build_lang_decl (func);
      func->csym = decl;
      func->builtin = BUILTINyes;

      members->push (func);
    }

  for (size_t i = 0; vec_safe_iterate (gcc_builtins_types, i, &decl); ++i)
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
      Type *t = build_frontend_type (TREE_TYPE (decl));

      /* Cannot create built-in type for DECL.  */
      if (!t)
	continue;

      members->push (build_alias_declaration (name, t));
    }

  /* Iterate through the target-specific builtin types for va_list.  */
  if (targetm.enum_va_list_p)
    {
      const char *name;
      tree type;

      for (int i = 0; targetm.enum_va_list_p (i, &name, &type); ++i)
	{
	  Type *t = build_frontend_type (type);
	  /* Cannot create built-in type.  */
	  if (!t)
	    continue;

	  members->push (build_alias_declaration (name, t));
	}
    }

  /* Push out declarations for any RECORD_TYPE types encountered when building
     all builtin functions and types.  */
  for (size_t i = 0; i < builtin_converted_decls.length (); ++i)
    {
      /* Currently, there is no need to run semantic, but we do want to output
	 initializers, typeinfo, and others on demand.  */
      Dsymbol *dsym = builtin_converted_decls[i].dsym;
      if (dsym != NULL)
	{
	  dsym->parent = m;
	  members->push (dsym);
	}
    }

  /* va_list should already be built, so no need to convert to D type again.  */
  members->push (build_alias_declaration ("__builtin_va_list", Type::tvalist));

  /* Expose target-specific integer types to the builtins module.  */
  {
    Type *t = build_frontend_type (long_integer_type_node);
    members->push (build_alias_declaration ("__builtin_clong", t));

    t = build_frontend_type (long_unsigned_type_node);
    members->push (build_alias_declaration ("__builtin_culong", t));

    t = build_frontend_type (long_long_integer_type_node);
    members->push (build_alias_declaration ("__builtin_clonglong", t));

    t = build_frontend_type (long_long_unsigned_type_node);
    members->push (build_alias_declaration ("__builtin_culonglong", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (byte_mode, 0));
    members->push (build_alias_declaration ("__builtin_machine_byte", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (byte_mode, 1));
    members->push (build_alias_declaration ("__builtin_machine_ubyte", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (word_mode, 0));
    members->push (build_alias_declaration ("__builtin_machine_int", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (word_mode, 1));
    members->push (build_alias_declaration ("__builtin_machine_uint", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (ptr_mode, 0));
    members->push (build_alias_declaration ("__builtin_pointer_int", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (ptr_mode, 1));
    members->push (build_alias_declaration ("__builtin_pointer_uint", t));

    /* _Unwind_Word has its own target specific mode.  */
    machine_mode mode = targetm.unwind_word_mode ();
    t = build_frontend_type (lang_hooks.types.type_for_mode (mode, 0));
    members->push (build_alias_declaration ("__builtin_unwind_int", t));

    t = build_frontend_type (lang_hooks.types.type_for_mode (mode, 1));
    members->push (build_alias_declaration ("__builtin_unwind_uint", t));
  }

  m->members->push (LinkDeclaration::create (LINKc, members));
}

/* Search for any `extern(C)' functions that match any known GCC library builtin
   function in D and override its internal back-end symbol.  */

static void
maybe_set_builtin_1 (Dsymbol *d)
{
  AttribDeclaration *ad = d->isAttribDeclaration ();
  FuncDeclaration *fd = d->isFuncDeclaration ();

  if (ad != NULL)
    {
      /* Recursively search through attribute decls.  */
      Dsymbols *decls = ad->include (NULL, NULL);
      if (decls && decls->dim)
	{
	  for (size_t i = 0; i < decls->dim; i++)
	    {
	      Dsymbol *sym = (*decls)[i];
	      maybe_set_builtin_1 (sym);
	    }
	}
    }
  else if (fd && !fd->fbody)
    {
      tree t;

      for (size_t i = 0; vec_safe_iterate (gcc_builtins_libfuncs, i, &t); ++i)
	{
	  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (t));

	  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));
	  if (fd->ident != Identifier::idPool (name))
	    continue;

	  /* Found a match, tell the frontend this is a builtin.  */
	  DECL_LANG_SPECIFIC (t) = build_lang_decl (fd);
	  fd->csym = t;
	  fd->builtin = BUILTINyes;
	  return;
	}
    }
}

/* A helper function for Target::loadModule.  Traverse all members in module M
   to search for any functions that can be mapped to any GCC builtin.  */

void
d_maybe_set_builtin (Module *m)
{
  if (!m || !m->members)
    return;

  for (size_t i = 0; i < m->members->dim; i++)
    {
      Dsymbol *sym = (*m->members)[i];
      maybe_set_builtin_1 (sym);
    }
}

/* Used to help initialize the builtin-types.def table.  When a type of
   the correct size doesn't exist, use error_mark_node instead of NULL.
   The latter results in segfaults even when a decl using the type doesn't
   get invoked.  */

static tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = lang_hooks.types.type_for_size (size, unsignedp);
  return type ? type : error_mark_node;
}

/* Support for DEF_BUILTIN.  */

static void
do_build_builtin_fn (built_in_function fncode,
		     const char *name,
		     built_in_class fnclass,
		     tree fntype, bool both_p, bool fallback_p,
		     tree fnattrs, bool implicit_p)
{
  tree decl;
  const char *libname;

  if (fntype == error_mark_node)
    return;

  gcc_assert ((!both_p && !fallback_p)
	      || !strncmp (name, "__builtin_",
			   strlen ("__builtin_")));

  libname = name + strlen ("__builtin_");

  decl = add_builtin_function (name, fntype, fncode, fnclass,
			       fallback_p ? libname : NULL, fnattrs);

  set_builtin_decl (fncode, decl, implicit_p);
}

/* Standard data types to be used in builtin argument declarations.  */

static GTY(()) tree string_type_node;
static GTY(()) tree const_string_type_node;
static GTY(()) tree wint_type_node;
static GTY(()) tree intmax_type_node;
static GTY(()) tree uintmax_type_node;
static GTY(()) tree signed_size_type_node;


/* Build nodes that would have been created by the C front-end; necessary
   for including builtin-types.def and ultimately builtins.def.  */

static void
d_build_c_type_nodes (void)
{
  void_list_node = build_tree_list (NULL_TREE, void_type_node);
  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_qualified_type (char_type_node,
						TYPE_QUAL_CONST));

  if (strcmp (SIZE_TYPE, "unsigned int") == 0)
    {
      intmax_type_node = integer_type_node;
      uintmax_type_node = unsigned_type_node;
      signed_size_type_node = integer_type_node;
    }
  else if (strcmp (SIZE_TYPE, "long unsigned int") == 0)
    {
      intmax_type_node = long_integer_type_node;
      uintmax_type_node = long_unsigned_type_node;
      signed_size_type_node = long_integer_type_node;
    }
  else if (strcmp (SIZE_TYPE, "long long unsigned int") == 0)
    {
      intmax_type_node = long_long_integer_type_node;
      uintmax_type_node = long_long_unsigned_type_node;
      signed_size_type_node = long_long_integer_type_node;
    }
  else
    gcc_unreachable ();

  wint_type_node = unsigned_type_node;
  pid_type_node = integer_type_node;
}

/* Build nodes that are used by the D front-end.
   These are distinct from C types.  */

static void
d_build_d_type_nodes (void)
{
  /* Integral types.  */
  d_byte_type = make_signed_type (8);
  d_ubyte_type = make_unsigned_type (8);

  d_short_type = make_signed_type (16);
  d_ushort_type = make_unsigned_type (16);

  d_int_type = make_signed_type (32);
  d_uint_type = make_unsigned_type (32);

  d_long_type = make_signed_type (64);
  d_ulong_type = make_unsigned_type (64);

  d_cent_type = make_signed_type (128);
  d_ucent_type = make_unsigned_type (128);

  {
    /* Re-define size_t as a D type.  */
    machine_mode type_mode = TYPE_MODE (size_type_node);
    size_type_node = lang_hooks.types.type_for_mode (type_mode, 1);
  }

  /* Bool and Character types.  */
  d_bool_type = make_unsigned_type (1);
  TREE_SET_CODE (d_bool_type, BOOLEAN_TYPE);

  char8_type_node = make_unsigned_type (8);
  TYPE_STRING_FLAG (char8_type_node) = 1;

  char16_type_node = make_unsigned_type (16);
  TYPE_STRING_FLAG (char16_type_node) = 1;

  char32_type_node = make_unsigned_type (32);
  TYPE_STRING_FLAG (char32_type_node) = 1;

  /* Imaginary types.  */
  ifloat_type_node = build_distinct_type_copy (float_type_node);
  TYPE_IMAGINARY_FLOAT (ifloat_type_node) = 1;

  idouble_type_node = build_distinct_type_copy (double_type_node);
  TYPE_IMAGINARY_FLOAT (idouble_type_node) = 1;

  ireal_type_node = build_distinct_type_copy (long_double_type_node);
  TYPE_IMAGINARY_FLOAT (ireal_type_node) = 1;

  /* Used for ModuleInfo, ClassInfo, and Interface decls.  */
  unknown_type_node = make_node (RECORD_TYPE);

  /* Make sure we get a unique function type, so we can give
     its pointer type a name.  (This wins for gdb).  */
  {
    tree vfunc_type = make_node (FUNCTION_TYPE);
    TREE_TYPE (vfunc_type) = d_int_type;
    TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
    layout_type (vfunc_type);

    vtable_entry_type = build_pointer_type (vfunc_type);
  }

  vtbl_ptr_type_node = build_pointer_type (vtable_entry_type);
  layout_type (vtbl_ptr_type_node);

  /* When an object is accessed via an interface, this type appears
     as the first entry in its vtable.  */
  {
    tree domain = build_index_type (size_int (3));
    vtbl_interface_type_node = build_array_type (ptr_type_node, domain);
  }

  /* Use `void[]' as a generic dynamic array type.  */
  array_type_node = make_struct_type ("__builtin_void[]", 2,
				      get_identifier ("length"), size_type_node,
				      get_identifier ("ptr"), ptr_type_node);
  TYPE_DYNAMIC_ARRAY (array_type_node) = 1;

  null_array_node = d_array_value (array_type_node, size_zero_node,
				   null_pointer_node);
}

/* Handle default attributes.  */

enum built_in_attribute
{
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_STRING(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
  ATTR_LAST
};

static GTY(()) tree built_in_attributes[(int) ATTR_LAST];

/* Initialize the attribute table for all the supported builtins.  */

static void
d_init_attributes (void)
{
  /* Fill in the built_in_attributes array.  */
#define DEF_ATTR_NULL_TREE(ENUM)	\
  built_in_attributes[(int) ENUM] = NULL_TREE;
# define DEF_ATTR_INT(ENUM, VALUE)	\
  built_in_attributes[(int) ENUM] = build_int_cst (NULL_TREE, VALUE);
#define DEF_ATTR_STRING(ENUM, VALUE)	\
  built_in_attributes[(int) ENUM] = build_string (strlen (VALUE), VALUE);
#define DEF_ATTR_IDENT(ENUM, STRING)	\
  built_in_attributes[(int) ENUM] = get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)	\
  built_in_attributes[(int) ENUM]			\
  = tree_cons (built_in_attributes[(int) PURPOSE],	\
	       built_in_attributes[(int) VALUE],	\
	       built_in_attributes[(int) CHAIN]);
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
}

/* Builtin types.  */

enum d_builtin_type
{
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) NAME,
#define DEF_FUNCTION_TYPE_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6) NAME,
#define DEF_FUNCTION_TYPE_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_8(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8) NAME,
#define DEF_FUNCTION_TYPE_9(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9) NAME,
#define DEF_FUNCTION_TYPE_10(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10) NAME,
#define DEF_FUNCTION_TYPE_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_VAR_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_VAR_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_5(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
				NAME,
#define DEF_FUNCTION_TYPE_VAR_6(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6) NAME,
#define DEF_FUNCTION_TYPE_VAR_7(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7) NAME,
#define DEF_FUNCTION_TYPE_VAR_11(NAME, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				 ARG6, ARG7, ARG8, ARG9, ARG10, ARG11) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_FUNCTION_TYPE_VAR_11
#undef DEF_POINTER_TYPE
  BT_LAST
};

typedef enum d_builtin_type builtin_type;

/* A temporary array used in communication with def_fn_type.  */
static GTY(()) tree builtin_types[(int) BT_LAST + 1];

/* A helper function for d_init_builtins.  Build function type for DEF with
   return type RET and N arguments.  If VAR is true, then the function should
   be variadic after those N arguments.

   Takes special care not to ICE if any of the types involved are
   error_mark_node, which indicates that said type is not in fact available
   (see builtin_type_for_size).  In which case the function type as a whole
   should be error_mark_node.  */

static void
def_fn_type (builtin_type def, builtin_type ret, bool var, int n, ...)
{
  tree t;
  tree *args = XALLOCAVEC (tree, n);
  va_list list;
  int i;

  va_start (list, n);
  for (i = 0; i < n; ++i)
    {
      builtin_type a = (builtin_type) va_arg (list, int);
      t = builtin_types[a];
      if (t == error_mark_node)
	goto egress;
      args[i] = t;
    }

  t = builtin_types[ret];
  if (t == error_mark_node)
    goto egress;
  if (var)
    t = build_varargs_function_type_array (t, n, args);
  else
    t = build_function_type_array (t, n, args);

 egress:
  builtin_types[def] = t;
  va_end (list);
}

/* Create builtin types and functions.  VA_LIST_REF_TYPE_NODE and
   VA_LIST_ARG_TYPE_NODE are used in builtin-types.def.  */

static void
d_define_builtins (tree va_list_ref_type_node ATTRIBUTE_UNUSED,
		   tree va_list_arg_type_node ATTRIBUTE_UNUSED)
{
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 0, 0);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 0, 1, ARG1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 0, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 0, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 0, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
  def_fn_type (ENUM, RETURN, 0, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
  def_fn_type (ENUM, RETURN, 0, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
  def_fn_type (ENUM, RETURN, 0, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
  def_fn_type (ENUM, RETURN, 0, 8, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
  def_fn_type (ENUM, RETURN, 0, 9, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8, ARG9);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9, ARG10)		 \
  def_fn_type (ENUM, RETURN, 0, 10, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8, ARG9, ARG10);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)	 \
  def_fn_type (ENUM, RETURN, 0, 11, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,  \
	       ARG7, ARG8, ARG9, ARG10, ARG11);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 1, 0);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 1, 1, ARG1);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 1, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 1, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 1, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
  def_fn_type (ENUM, RETURN, 1, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6)					    \
  def_fn_type (ENUM, RETURN, 1, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				    \
  def_fn_type (ENUM, RETURN, 1, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_VAR_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				 ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)       \
  def_fn_type (ENUM, RETURN, 1, 11, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,      \
	       ARG7, ARG8, ARG9, ARG10, ARG11);
#define DEF_POINTER_TYPE(ENUM, TYPE) \
  builtin_types[(int) ENUM] = build_pointer_type (builtin_types[(int) TYPE]);

#include "builtin-types.def"

#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_FUNCTION_TYPE_VAR_11
#undef DEF_POINTER_TYPE
  builtin_types[(int) BT_LAST] = NULL_TREE;

  d_init_attributes ();

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, FALLBACK_P, \
		    NONANSI_P, ATTRS, IMPLICIT, COND)			  \
  if (NAME && COND)							  \
    do_build_builtin_fn (ENUM, NAME, CLASS,				  \
			 builtin_types[(int) TYPE],			  \
			 BOTH_P, FALLBACK_P,				  \
			 built_in_attributes[(int) ATTRS], IMPLICIT);
#include "builtins.def"
#undef DEF_BUILTIN
}

/* Build builtin functions and types for the D language frontend.  */

void
d_init_builtins (void)
{
  /* Build the "standard" abi va_list.  */
  Type::tvalist = build_frontend_type (va_list_type_node);
  if (!Type::tvalist)
    {
      error ("cannot represent built-in va_list type in D");
      gcc_unreachable ();
    }

  /* Map the va_list type to the D frontend Type.  This is to prevent both
     errors in gimplification or an ICE in targetm.canonical_va_list_type.  */
  Type::tvalist->ctype = va_list_type_node;
  TYPE_LANG_SPECIFIC (va_list_type_node) = build_lang_type (Type::tvalist);

  d_build_c_type_nodes ();
  d_build_d_type_nodes ();

  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      /* It might seem natural to make the argument type a pointer, but there
	 is no implicit casting from arrays to pointers in D.  */
      d_define_builtins (va_list_type_node, va_list_type_node);
    }
  else
    {
      d_define_builtins (build_reference_type (va_list_type_node),
			 va_list_type_node);
    }

  targetm.init_builtins ();
  build_common_builtin_nodes ();
}

/* Registration of machine- or os-specific builtin types.
   Add to builtin types list for maybe processing later
   if `gcc.builtins' was imported into the current module.  */

void
d_register_builtin_type (tree type, const char *name)
{
  tree decl = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
			  get_identifier (name), type);
  DECL_ARTIFICIAL (decl) = 1;

  if (!TYPE_NAME (type))
    TYPE_NAME (type) = decl;

  vec_safe_push (gcc_builtins_types, decl);
}

/* Add DECL to builtin functions list for maybe processing later
   if `gcc.builtins' was imported into the current module.  */

tree
d_builtin_function (tree decl)
{
  if (!flag_no_builtin && DECL_ASSEMBLER_NAME_SET_P (decl))
    vec_safe_push (gcc_builtins_libfuncs, decl);

  vec_safe_push (gcc_builtins_functions, decl);
  return decl;
}


#include "gt-d-d-builtins.h"
