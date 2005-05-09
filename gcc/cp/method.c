/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Handle method declarations.  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "rtl.h"
#include "expr.h"
#include "output.h"
#include "flags.h"
#include "toplev.h"
#include "tm_p.h"
#include "target.h"

/* Various flags to control the mangling process.  */

enum mangling_flags
{
  /* No flags.  */
  mf_none = 0,
  /* The thing we are presently mangling is part of a template type,
     rather than a fully instantiated type.  Therefore, we may see
     complex expressions where we would normally expect to see a
     simple integer constant.  */
  mf_maybe_uninstantiated = 1,
  /* When mangling a numeric value, use the form `_XX_' (instead of
     just `XX') if the value has more than one digit.  */
  mf_use_underscores_around_value = 2
};

typedef enum mangling_flags mangling_flags;

static tree thunk_adjust (tree, bool, HOST_WIDE_INT, tree);
static void do_build_assign_ref (tree);
static void do_build_copy_constructor (tree);
static tree synthesize_exception_spec (tree, tree (*) (tree, void *), void *);
static tree locate_dtor (tree, void *);
static tree locate_ctor (tree, void *);
static tree locate_copy (tree, void *);
#ifdef ASM_OUTPUT_DEF
static tree make_alias_for_thunk (tree);
#endif

/* Called once to initialize method.c.  */

void
init_method (void)
{
  init_mangle ();
}


/* Set the mangled name (DECL_ASSEMBLER_NAME) for DECL.  */

void
set_mangled_name_for_decl (tree decl)
{
  if (processing_template_decl)
    /* There's no need to mangle the name of a template function.  */
    return;

  mangle_decl (decl);
}


/* Return a this or result adjusting thunk to FUNCTION.  THIS_ADJUSTING
   indicates whether it is a this or result adjusting thunk.
   FIXED_OFFSET and VIRTUAL_OFFSET indicate how to do the adjustment
   (see thunk_adjust).  VIRTUAL_OFFSET can be NULL, but FIXED_OFFSET
   never is.  VIRTUAL_OFFSET is the /index/ into the vtable for this
   adjusting thunks, we scale it to a byte offset. For covariant
   thunks VIRTUAL_OFFSET is the virtual binfo.  You must post process
   the returned thunk with finish_thunk.  */

tree
make_thunk (tree function, bool this_adjusting,
	    tree fixed_offset, tree virtual_offset)
{
  HOST_WIDE_INT d;
  tree thunk;
  
  my_friendly_assert (TREE_CODE (function) == FUNCTION_DECL, 20021025);
  /* We can have this thunks to covariant thunks, but not vice versa.  */
  my_friendly_assert (!DECL_THIS_THUNK_P (function), 20021127);
  my_friendly_assert (!DECL_RESULT_THUNK_P (function) || this_adjusting,
		      20031123);
  
  /* Scale the VIRTUAL_OFFSET to be in terms of bytes.  */
  if (this_adjusting && virtual_offset)
    virtual_offset 
      = size_binop (MULT_EXPR,
 		    virtual_offset,
  		    convert (ssizetype,
  			     TYPE_SIZE_UNIT (vtable_entry_type)));
  
  d = tree_low_cst (fixed_offset, 0);
  
  /* See if we already have the thunk in question.  For this_adjusting
     thunks VIRTUAL_OFFSET will be an INTEGER_CST, for covariant thunks it
     will be a BINFO.  */
  for (thunk = DECL_THUNKS (function); thunk; thunk = TREE_CHAIN (thunk))
    if (DECL_THIS_THUNK_P (thunk) == this_adjusting
	&& THUNK_FIXED_OFFSET (thunk) == d
	&& !virtual_offset == !THUNK_VIRTUAL_OFFSET (thunk)
	&& (!virtual_offset
	    || (this_adjusting
		? tree_int_cst_equal (THUNK_VIRTUAL_OFFSET (thunk),
				      virtual_offset)
		: THUNK_VIRTUAL_OFFSET (thunk) == virtual_offset)))
      return thunk;
  
  /* All thunks must be created before FUNCTION is actually emitted;
     the ABI requires that all thunks be emitted together with the
     function to which they transfer control.  */
  my_friendly_assert (!TREE_ASM_WRITTEN (function), 20021025);
  /* Likewise, we can only be adding thunks to a function declared in
     the class currently being laid out.  */
  my_friendly_assert (TYPE_SIZE (DECL_CONTEXT (function))
		      && TYPE_BEING_DEFINED (DECL_CONTEXT (function)),
		      20031211);

  thunk = build_decl (FUNCTION_DECL, NULL_TREE, TREE_TYPE (function));
  DECL_LANG_SPECIFIC (thunk) = DECL_LANG_SPECIFIC (function);
  cxx_dup_lang_specific_decl (thunk);
  DECL_THUNKS (thunk) = NULL_TREE;
  
  DECL_CONTEXT (thunk) = DECL_CONTEXT (function);
  TREE_READONLY (thunk) = TREE_READONLY (function);
  TREE_THIS_VOLATILE (thunk) = TREE_THIS_VOLATILE (function);
  TREE_PUBLIC (thunk) = TREE_PUBLIC (function);
  if (flag_weak)
    comdat_linkage (thunk);
  SET_DECL_THUNK_P (thunk, this_adjusting);
  THUNK_TARGET (thunk) = function;
  THUNK_FIXED_OFFSET (thunk) = d;
  THUNK_VIRTUAL_OFFSET (thunk) = virtual_offset;
  THUNK_ALIAS (thunk) = NULL_TREE;
  
  /* The thunk itself is not a constructor or destructor, even if
     the thing it is thunking to is.  */
  DECL_INTERFACE_KNOWN (thunk) = 1;
  DECL_NOT_REALLY_EXTERN (thunk) = 1;
  DECL_SAVED_FUNCTION_DATA (thunk) = NULL;
  DECL_DESTRUCTOR_P (thunk) = 0;
  DECL_CONSTRUCTOR_P (thunk) = 0;
  /* And neither is it a clone.  */
  DECL_CLONED_FUNCTION (thunk) = NULL_TREE;
  DECL_EXTERNAL (thunk) = 1;
  DECL_ARTIFICIAL (thunk) = 1;
  /* Even if this thunk is a member of a local class, we don't
     need a static chain.  */
  DECL_NO_STATIC_CHAIN (thunk) = 1;
  /* The THUNK is not a pending inline, even if the FUNCTION is.  */
  DECL_PENDING_INLINE_P (thunk) = 0;
  DECL_INLINE (thunk) = 0;
  DECL_DECLARED_INLINE_P (thunk) = 0;
  /* Nor has it been deferred.  */
  DECL_DEFERRED_FN (thunk) = 0;
  
  /* Add it to the list of thunks associated with FUNCTION.  */
  TREE_CHAIN (thunk) = DECL_THUNKS (function);
  DECL_THUNKS (function) = thunk;

  return thunk;
}

/* Finish THUNK, a thunk decl.  */

void
finish_thunk (tree thunk)
{
  tree function, name;
  tree fixed_offset = ssize_int (THUNK_FIXED_OFFSET (thunk));
  tree virtual_offset = THUNK_VIRTUAL_OFFSET (thunk);

  my_friendly_assert (!DECL_NAME (thunk) && DECL_THUNK_P (thunk), 20021127);
  if (virtual_offset && DECL_RESULT_THUNK_P (thunk))
    virtual_offset = BINFO_VPTR_FIELD (virtual_offset);
  function = THUNK_TARGET (thunk);
  name = mangle_thunk (function, DECL_THIS_THUNK_P (thunk),
		       fixed_offset, virtual_offset);

  /* We can end up with declarations of (logically) different
     covariant thunks, that do identical adjustments.  The two thunks
     will be adjusting between within different hierarchies, which
     happen to have the same layout.  We must nullify one of them to
     refer to the other.  */
  if (DECL_RESULT_THUNK_P (thunk))
    {
      tree cov_probe;

      for (cov_probe = DECL_THUNKS (function);
	   cov_probe; cov_probe = TREE_CHAIN (cov_probe))
	if (DECL_NAME (cov_probe) == name)
	  {
	    my_friendly_assert (!DECL_THUNKS (thunk), 20031023);
	    THUNK_ALIAS (thunk) = (THUNK_ALIAS (cov_probe)
				   ? THUNK_ALIAS (cov_probe) : cov_probe);
	    break;
	  }
    }
  
  DECL_NAME (thunk) = name;
  SET_DECL_ASSEMBLER_NAME (thunk, name);
}

/* Adjust PTR by the constant FIXED_OFFSET, and by the vtable
   offset indicated by VIRTUAL_OFFSET, if that is
   non-null. THIS_ADJUSTING is nonzero for a this adjusting thunk and
   zero for a result adjusting thunk.  */

static tree
thunk_adjust (tree ptr, bool this_adjusting,
	      HOST_WIDE_INT fixed_offset, tree virtual_offset)
{
  if (this_adjusting)
    /* Adjust the pointer by the constant.  */
    ptr = fold (build (PLUS_EXPR, TREE_TYPE (ptr), ptr,
		       ssize_int (fixed_offset)));

  /* If there's a virtual offset, look up that value in the vtable and
     adjust the pointer again.  */
  if (virtual_offset)
    {
      tree vtable;

      ptr = save_expr (ptr);
      /* The vptr is always at offset zero in the object.  */
      vtable = build1 (NOP_EXPR,
		       build_pointer_type (build_pointer_type 
					   (vtable_entry_type)),
		       ptr);
      /* Form the vtable address.  */
      vtable = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (vtable)), vtable);
      /* Find the entry with the vcall offset.  */
      vtable = build (PLUS_EXPR, TREE_TYPE (vtable), vtable, virtual_offset);
      /* Get the offset itself.  */
      vtable = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (vtable)), vtable);
      /* Adjust the `this' pointer.  */
      ptr = fold (build (PLUS_EXPR, TREE_TYPE (ptr), ptr, vtable));
    }
  
  if (!this_adjusting)
    /* Adjust the pointer by the constant.  */
    ptr = fold (build (PLUS_EXPR, TREE_TYPE (ptr), ptr,
		       ssize_int (fixed_offset)));

  return ptr;
}

/* Garbage collector tables contains thunk_labelno even when places
   inside ifdef block.  */
static GTY (()) int thunk_labelno;
#ifdef ASM_OUTPUT_DEF

/* Create a static alias to function.  */

static tree
make_alias_for_thunk (tree function)
{
  tree alias;
  char buf[256];

#if defined (TARGET_IS_PE_COFF)
  if (DECL_ONE_ONLY (function))
    return function;
#endif

  ASM_GENERATE_INTERNAL_LABEL (buf, "LTHUNK", thunk_labelno);
  thunk_labelno++;
  alias = build_decl (FUNCTION_DECL, get_identifier (buf),
		      TREE_TYPE (function));
  DECL_LANG_SPECIFIC (alias) = DECL_LANG_SPECIFIC (function);
  cxx_dup_lang_specific_decl (alias);
  DECL_CONTEXT (alias) = NULL;
  TREE_READONLY (alias) = TREE_READONLY (function);
  TREE_THIS_VOLATILE (alias) = TREE_THIS_VOLATILE (function);
  TREE_PUBLIC (alias) = 0;
  DECL_INTERFACE_KNOWN (alias) = 1;
  DECL_NOT_REALLY_EXTERN (alias) = 1;
  DECL_THIS_STATIC (alias) = 1;
  DECL_SAVED_FUNCTION_DATA (alias) = NULL;
  DECL_DESTRUCTOR_P (alias) = 0;
  DECL_CONSTRUCTOR_P (alias) = 0;
  DECL_CLONED_FUNCTION (alias) = NULL_TREE;
  DECL_EXTERNAL (alias) = 0;
  DECL_ARTIFICIAL (alias) = 1;
  DECL_NO_STATIC_CHAIN (alias) = 1;
  DECL_PENDING_INLINE_P (alias) = 0;
  DECL_INLINE (alias) = 0;
  DECL_DECLARED_INLINE_P (alias) = 0;
  DECL_DEFERRED_FN (alias) = 0;
  DECL_USE_TEMPLATE (alias) = 0;
  DECL_TEMPLATE_INSTANTIATED (alias) = 0;
  DECL_TEMPLATE_INFO (alias) = NULL;
  DECL_INITIAL (alias) = error_mark_node;
  TREE_ADDRESSABLE (alias) = 1;
  TREE_USED (alias) = 1;
  SET_DECL_ASSEMBLER_NAME (alias, DECL_NAME (alias));
  TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (alias)) = 1;
  if (!flag_syntax_only)
    assemble_alias (alias, DECL_ASSEMBLER_NAME (function));
  return alias;
}
#endif

/* Emit the definition of a C++ multiple inheritance or covariant
   return vtable thunk.  If EMIT_P is nonzero, the thunk is emitted
   immediately.  */

void
use_thunk (tree thunk_fndecl, bool emit_p)
{
  tree function, alias;
  tree virtual_offset;
  HOST_WIDE_INT fixed_offset, virtual_value;
  bool this_adjusting = DECL_THIS_THUNK_P (thunk_fndecl);

  /* We should have called finish_thunk to give it a name.  */
  my_friendly_assert (DECL_NAME (thunk_fndecl), 20021127);

  /* We should never be using an alias, always refer to the
     aliased thunk.  */
  my_friendly_assert (!THUNK_ALIAS (thunk_fndecl), 20031023);

  if (TREE_ASM_WRITTEN (thunk_fndecl))
    return;
  
  function = THUNK_TARGET (thunk_fndecl);
  if (DECL_RESULT (thunk_fndecl))
    /* We already turned this thunk into an ordinary function.
       There's no need to process this thunk again.  */
    return;

  if (DECL_THUNK_P (function))
    /* The target is itself a thunk, process it now.  */
    use_thunk (function, emit_p);
  
  /* Thunks are always addressable; they only appear in vtables.  */
  TREE_ADDRESSABLE (thunk_fndecl) = 1;

  /* Figure out what function is being thunked to.  It's referenced in
     this translation unit.  */
  TREE_ADDRESSABLE (function) = 1;
  mark_used (function);
  if (!emit_p)
    return;

#ifdef ASM_OUTPUT_DEF
  alias = make_alias_for_thunk (function);
#else
  alias = function;
#endif

  fixed_offset = THUNK_FIXED_OFFSET (thunk_fndecl);
  virtual_offset = THUNK_VIRTUAL_OFFSET (thunk_fndecl);

  if (virtual_offset)
    {
      if (!this_adjusting)
	virtual_offset = BINFO_VPTR_FIELD (virtual_offset);
      virtual_value = tree_low_cst (virtual_offset, /*pos=*/0);
      my_friendly_assert (virtual_value, 20021026);
    }
  else
    virtual_value = 0;
  
  /* And, if we need to emit the thunk, it's used.  */
  mark_used (thunk_fndecl);
  /* This thunk is actually defined.  */
  DECL_EXTERNAL (thunk_fndecl) = 0;
  /* The linkage of the function may have changed.  FIXME in linkage
     rewrite.  */
  TREE_PUBLIC (thunk_fndecl) = TREE_PUBLIC (function);
  DECL_VISIBILITY (thunk_fndecl) = DECL_VISIBILITY (function);

  if (flag_syntax_only)
    {
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      return;
    }

  push_to_top_level ();

#if defined (ASM_OUTPUT_DEF) \
  && !defined (TARGET_IS_PE_COFF)
  if (targetm.have_named_sections)
    {
      resolve_unique_section (function, 0, flag_function_sections);

      if (DECL_SECTION_NAME (function) != NULL && DECL_ONE_ONLY (function))
	{
	  resolve_unique_section (thunk_fndecl, 0, flag_function_sections);

	  /* Output the thunk into the same section as function.  */
	  DECL_SECTION_NAME (thunk_fndecl) = DECL_SECTION_NAME (function);
	}
    }
#endif

  /* The back-end expects DECL_INITIAL to contain a BLOCK, so we
     create one.  */
  DECL_INITIAL (thunk_fndecl) = make_node (BLOCK);
  BLOCK_VARS (DECL_INITIAL (thunk_fndecl)) = DECL_ARGUMENTS (thunk_fndecl);
  
  if (this_adjusting
      && targetm.asm_out.can_output_mi_thunk (thunk_fndecl, fixed_offset,
					      virtual_value, alias))
    {
      const char *fnname;
      current_function_decl = thunk_fndecl;
      DECL_RESULT (thunk_fndecl)
	= build_decl (RESULT_DECL, 0, integer_type_node);
      fnname = XSTR (XEXP (DECL_RTL (thunk_fndecl), 0), 0);
      init_function_start (thunk_fndecl);
      current_function_is_thunk = 1;
      assemble_start_function (thunk_fndecl, fnname);

      targetm.asm_out.output_mi_thunk (asm_out_file, thunk_fndecl,
				       fixed_offset, virtual_value, alias);

      assemble_end_function (thunk_fndecl, fnname);
      current_function_decl = 0;
      cfun = 0;
      /* Because init_function_start increments this, we must
	 decrement it.  */
      immediate_size_expand--;
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
    }
  else
    {
      /* If this is a covariant thunk, or we don't have the necessary
	 code for efficient thunks, generate a thunk function that
	 just makes a call to the real function.  Unfortunately, this
	 doesn't work for varargs.  */

      tree a, t;

      if (varargs_function_p (function))
	error ("generic thunk code fails for method `%#D' which uses `...'",
	       function);

      /* Set up cloned argument trees for the thunk.  */
      t = NULL_TREE;
      for (a = DECL_ARGUMENTS (function); a; a = TREE_CHAIN (a))
	{
	  tree x = copy_node (a);
	  TREE_CHAIN (x) = t;
	  DECL_CONTEXT (x) = thunk_fndecl;
	  SET_DECL_RTL (x, NULL_RTX);
	  t = x;
	}
      a = nreverse (t);
      DECL_ARGUMENTS (thunk_fndecl) = a;
      DECL_RESULT (thunk_fndecl) = NULL_TREE;

      start_function (NULL_TREE, thunk_fndecl, NULL_TREE, SF_PRE_PARSED);
      /* We don't bother with a body block for thunks.  */

      /* There's no need to check accessibility inside the thunk body.  */
      push_deferring_access_checks (dk_no_check);

      t = a;
      if (this_adjusting)
	t = thunk_adjust (t, /*this_adjusting=*/1,
			  fixed_offset, virtual_offset);
      
      /* Build up the call to the real function.  */
      t = tree_cons (NULL_TREE, t, NULL_TREE);
      for (a = TREE_CHAIN (a); a; a = TREE_CHAIN (a))
	t = tree_cons (NULL_TREE, a, t);
      t = nreverse (t);
      t = build_call (alias, t);
      CALL_FROM_THUNK_P (t) = 1;
      t = force_target_expr (TREE_TYPE (t), t);
      if (!this_adjusting)
	t = thunk_adjust (t, /*this_adjusting=*/0,
			  fixed_offset, virtual_offset);
      
      if (VOID_TYPE_P (TREE_TYPE (t)))
	finish_expr_stmt (t);
      else
	finish_return_stmt (t);

      /* Since we want to emit the thunk, we explicitly mark its name as
	 referenced.  */
      mark_referenced (DECL_ASSEMBLER_NAME (thunk_fndecl));

      /* But we don't want debugging information about it.  */
      DECL_IGNORED_P (thunk_fndecl) = 1;

      /* Re-enable access control.  */
      pop_deferring_access_checks ();

      expand_body (finish_function (0));
    }

  pop_from_top_level ();
}

/* Code for synthesizing methods which have default semantics defined.  */

/* Generate code for default X(X&) constructor.  */

static void
do_build_copy_constructor (tree fndecl)
{
  tree parm = FUNCTION_FIRST_USER_PARM (fndecl);
  tree t;

  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type)
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type))
    {
      t = build (INIT_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      tree member_init_list = NULL_TREE;
      int cvquals = cp_type_quals (TREE_TYPE (parm));
      int i;

      /* Initialize all the base-classes with the parameter converted
	 to their type so that we get their copy constructor and not
	 another constructor that takes current_class_type.  We must
	 deal with the binfo's directly as a direct base might be
	 inaccessible due to ambiguity.  */
      for (t = CLASSTYPE_VBASECLASSES (current_class_type); t;
	   t = TREE_CHAIN (t))
	{
	  tree binfo = TREE_VALUE (t);
	  
	  member_init_list 
	    = tree_cons (binfo,
			 build_tree_list (NULL_TREE,
					  build_base_path (PLUS_EXPR, parm,
							   binfo, 1)),
			 member_init_list);
	}

      for (i = 0; i < n_bases; ++i)
	{
	  tree binfo = TREE_VEC_ELT (binfos, i);
	  if (TREE_VIA_VIRTUAL (binfo))
	    continue; 

	  member_init_list 
	    = tree_cons (binfo,
			 build_tree_list (NULL_TREE,
					  build_base_path (PLUS_EXPR, parm,
							   binfo, 1)),
			 member_init_list);
	}

      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree init = parm;
	  tree field = fields;
	  tree expr_type;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  expr_type = TREE_TYPE (field);
	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if (ANON_AGGR_TYPE_P (expr_type) && TYPE_FIELDS (expr_type))
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops.  */;
	  else
	    continue;

	  /* Compute the type of "init->field".  If the copy-constructor
	     parameter is, for example, "const S&", and the type of
	     the field is "T", then the type will usually be "const
	     T".  (There are no cv-qualified variants of reference
	     types.)  */
	  if (TREE_CODE (expr_type) != REFERENCE_TYPE)
	    {
	      int quals = cvquals;
	      
	      if (DECL_MUTABLE_P (field))
		quals &= ~TYPE_QUAL_CONST;
	      expr_type = cp_build_qualified_type (expr_type, quals);
	    }
	  
	  init = build (COMPONENT_REF, expr_type, init, field);
	  init = build_tree_list (NULL_TREE, init);

	  member_init_list = tree_cons (field, init, member_init_list);
	}
      finish_mem_initializers (member_init_list);
    }
}

static void
do_build_assign_ref (tree fndecl)
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree compound_stmt;

  compound_stmt = begin_compound_stmt (/*has_no_scope=*/false);
  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type)
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type))
    {
      tree t = build (MODIFY_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields;
      int cvquals = cp_type_quals (TREE_TYPE (parm));
      int i;

      /* Assign to each of the direct base classes.  */
      for (i = 0; i < CLASSTYPE_N_BASECLASSES (current_class_type); ++i)
	{
	  tree binfo;
	  tree converted_parm;

	  binfo = BINFO_BASETYPE (TYPE_BINFO (current_class_type), i);
	  /* We must convert PARM directly to the base class
	     explicitly since the base class may be ambiguous.  */
	  converted_parm = build_base_path (PLUS_EXPR, parm, binfo, 1);
	  /* Call the base class assignment operator.  */
	  finish_expr_stmt 
	    (build_special_member_call (current_class_ref, 
					ansi_assopname (NOP_EXPR),
					build_tree_list (NULL_TREE, 
							 converted_parm),
					binfo,
					LOOKUP_NORMAL | LOOKUP_NONVIRTUAL));
	}

      /* Assign to each of the non-static data members.  */
      for (fields = TYPE_FIELDS (current_class_type); 
	   fields; 
	   fields = TREE_CHAIN (fields))
	{
	  tree comp = current_class_ref;
	  tree init = parm;
	  tree field = fields;
	  tree expr_type;
	  int quals;

	  if (TREE_CODE (field) != FIELD_DECL || DECL_ARTIFICIAL (field))
	    continue;

	  expr_type = TREE_TYPE (field);
	  if (CP_TYPE_CONST_P (expr_type))
	    {
              error ("non-static const member `%#D', can't use default assignment operator", field);
	      continue;
	    }
	  else if (TREE_CODE (expr_type) == REFERENCE_TYPE)
	    {
	      error ("non-static reference member `%#D', can't use default assignment operator", field);
	      continue;
	    }

	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if (ANON_AGGR_TYPE_P (expr_type) && TYPE_FIELDS (expr_type))
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops.  */;
	  else
	    continue;

	  comp = build (COMPONENT_REF, TREE_TYPE (field), comp, field);

	  /* Compute the type of init->field  */
	  quals = cvquals;
	  if (DECL_MUTABLE_P (field))
	    quals &= ~TYPE_QUAL_CONST;
	  expr_type = cp_build_qualified_type (expr_type, quals);
	  
	  init = build (COMPONENT_REF, expr_type, init, field);

	  if (DECL_NAME (field))
	    init = build_modify_expr (comp, NOP_EXPR, init);
	  else
	    init = build (MODIFY_EXPR, TREE_TYPE (comp), comp, init);
	  finish_expr_stmt (init);
	}
    }
  finish_return_stmt (current_class_ref);
  finish_compound_stmt (compound_stmt);
}

void
synthesize_method (tree fndecl)
{
  bool nested = (current_function_decl != NULL_TREE);
  tree context = decl_function_context (fndecl);
  bool need_body = true;
  tree stmt;

  if (at_eof)
    import_export_decl (fndecl);

  /* If we've been asked to synthesize a clone, just synthesize the
     cloned function instead.  Doing so will automatically fill in the
     body for the clone.  */
  if (DECL_CLONED_FUNCTION_P (fndecl))
    {
      synthesize_method (DECL_CLONED_FUNCTION (fndecl));
      return;
    }

  /* We may be in the middle of deferred access check.  Disable
     it now.  */
  push_deferring_access_checks (dk_no_deferred);

  if (! context)
    push_to_top_level ();
  else if (nested)
    push_function_context_to (context);

  /* Put the function definition at the position where it is needed,
     rather than within the body of the class.  That way, an error
     during the generation of the implicit body points at the place
     where the attempt to generate the function occurs, giving the
     user a hint as to why we are attempting to generate the
     function.  */
  DECL_SOURCE_LOCATION (fndecl) = input_location;

  interface_unknown = 1;
  start_function (NULL_TREE, fndecl, NULL_TREE, SF_DEFAULT | SF_PRE_PARSED);
  clear_last_expr ();
  stmt = begin_function_body ();

  if (DECL_OVERLOADED_OPERATOR_P (fndecl) == NOP_EXPR)
    {
      do_build_assign_ref (fndecl);
      need_body = false;
    }
  else if (DECL_CONSTRUCTOR_P (fndecl))
    {
      tree arg_chain = FUNCTION_FIRST_USER_PARMTYPE (fndecl);
      if (arg_chain != void_list_node)
	do_build_copy_constructor (fndecl);
      else if (TYPE_NEEDS_CONSTRUCTING (current_class_type))
	finish_mem_initializers (NULL_TREE);
    }

  /* If we haven't yet generated the body of the function, just
     generate an empty compound statement.  */
  if (need_body)
    {
      tree compound_stmt;
      compound_stmt = begin_compound_stmt (/*has_no_scope=*/false);
      finish_compound_stmt (compound_stmt);
    }

  finish_function_body (stmt);
  expand_or_defer_fn (finish_function (0));

  extract_interface_info ();
  if (! context)
    pop_from_top_level ();
  else if (nested)
    pop_function_context_from (context);

  pop_deferring_access_checks ();
}

/* Use EXTRACTOR to locate the relevant function called for each base &
   class field of TYPE. CLIENT allows additional information to be passed
   to EXTRACTOR.  Generates the union of all exceptions generated by those
   functions.  Note that we haven't updated TYPE_FIELDS and such of any
   variants yet, so we need to look at the main one.  */

static tree
synthesize_exception_spec (tree type, tree (*extractor) (tree, void*),
                           void *client)
{
  tree raises = empty_except_spec;
  tree fields = TYPE_FIELDS (type);
  int i, n_bases = CLASSTYPE_N_BASECLASSES (type);
  tree binfos = TYPE_BINFO_BASETYPES (type);

  for (i = 0; i != n_bases; i++)
    {
      tree base = BINFO_TYPE (TREE_VEC_ELT (binfos, i));
      tree fn = (*extractor) (base, client);
      if (fn)
        {
          tree fn_raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn));
          
          raises = merge_exception_specifiers (raises, fn_raises);
        }
    }
  for (; fields; fields = TREE_CHAIN (fields))
    {
      tree type = TREE_TYPE (fields);
      tree fn;
      
      if (TREE_CODE (fields) != FIELD_DECL || DECL_ARTIFICIAL (fields))
        continue;
      while (TREE_CODE (type) == ARRAY_TYPE)
  	type = TREE_TYPE (type);
      if (TREE_CODE (type) != RECORD_TYPE)
        continue;
      
      fn = (*extractor) (type, client);
      if (fn)
        {
          tree fn_raises = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (fn));
          
          raises = merge_exception_specifiers (raises, fn_raises);
        }
    }
  return raises;
}

/* Locate the dtor of TYPE.  */

static tree
locate_dtor (tree type, void *client ATTRIBUTE_UNUSED)
{
  tree fns;
  
  if (!TYPE_HAS_DESTRUCTOR (type))
    return NULL_TREE;
  fns = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type),
                      CLASSTYPE_DESTRUCTOR_SLOT);
  return fns;
}

/* Locate the default ctor of TYPE.  */

static tree
locate_ctor (tree type, void *client ATTRIBUTE_UNUSED)
{
  tree fns;
  
  if (!TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
    return NULL_TREE;
  
  fns = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type),
                      CLASSTYPE_CONSTRUCTOR_SLOT);
  for (; fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      tree parms = TYPE_ARG_TYPES (TREE_TYPE (fn));
      
      if (sufficient_parms_p (TREE_CHAIN (parms)))
        return fn;
    }
  return NULL_TREE;
}

struct copy_data
{
  tree name;
  int quals;
};

/* Locate the copy ctor or copy assignment of TYPE. CLIENT_
   points to a COPY_DATA holding the name (NULL for the ctor)
   and desired qualifiers of the source operand.  */

static tree
locate_copy (tree type, void *client_)
{
  struct copy_data *client = (struct copy_data *)client_;
  tree fns;
  int ix = -1;
  tree best = NULL_TREE;
  bool excess_p = false;
  
  if (client->name)
    {
      if (TYPE_HAS_ASSIGN_REF (type))
        ix = lookup_fnfields_1 (type, client->name);
    }
  else if (TYPE_HAS_INIT_REF (type))
    ix = CLASSTYPE_CONSTRUCTOR_SLOT;
  if (ix < 0)
    return NULL_TREE;
  fns = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), ix);
  
  for (; fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
      tree parms = TYPE_ARG_TYPES (TREE_TYPE (fn));
      tree src_type;
      int excess;
      int quals;
      
      parms = TREE_CHAIN (parms);
      if (!parms)
        continue;
      src_type = non_reference (TREE_VALUE (parms));
      if (!same_type_ignoring_top_level_qualifiers_p (src_type, type))
        continue;
      if (!sufficient_parms_p (TREE_CHAIN (parms)))
        continue;
      quals = cp_type_quals (src_type);
      if (client->quals & ~quals)
        continue;
      excess = quals & ~client->quals;
      if (!best || (excess_p && !excess))
        {
          best = fn;
          excess_p = excess;
        }
      else
        /* Ambiguous */
        return NULL_TREE;
    }
  return best;
}

/* Implicitly declare the special function indicated by KIND, as a
   member of TYPE.  For copy constructors and assignment operators,
   CONST_P indicates whether these functions should take a const
   reference argument or a non-const reference.  */

tree
implicitly_declare_fn (special_function_kind kind, tree type, bool const_p)
{
  tree declspecs = NULL_TREE;
  tree fn, args = NULL_TREE;
  tree raises = empty_except_spec;
  bool retref = false;
  bool has_parm = false;
  tree name = constructor_name (type);

  switch (kind)
    {
    case sfk_destructor:
      /* Destructor.  */
      name = build_nt (BIT_NOT_EXPR, name);
      args = void_list_node;
      raises = synthesize_exception_spec (type, &locate_dtor, 0);
      break;

    case sfk_constructor:
      /* Default constructor.  */
      args = void_list_node;
      raises = synthesize_exception_spec (type, &locate_ctor, 0);
      break;

    case sfk_copy_constructor:
    case sfk_assignment_operator:
    {
      struct copy_data data;
      tree argtype = type;
      
      has_parm = true;
      data.name = NULL;
      data.quals = 0;
      if (kind == sfk_assignment_operator)
        {
          retref = true;
          declspecs = build_tree_list (NULL_TREE, type);

          name = ansi_assopname (NOP_EXPR);
          data.name = name;
        }
      if (const_p)
        {
          data.quals = TYPE_QUAL_CONST;
          argtype = build_qualified_type (argtype, TYPE_QUAL_CONST);
        }
    
      argtype = build_reference_type (argtype);
      args = build_tree_list (hash_tree_chain (argtype, NULL_TREE),
			      get_identifier ("_ctor_arg"));
      args = tree_cons (NULL_TREE, args, void_list_node);
      
      raises = synthesize_exception_spec (type, &locate_copy, &data);
      break;
    }
    default:
      abort ();
    }

  TREE_PARMLIST (args) = 1;

  {
    tree declarator = make_call_declarator (name, args, NULL_TREE, raises);
    
    if (retref)
      declarator = build_nt (ADDR_EXPR, declarator);

    fn = grokfield (declarator, declspecs, NULL_TREE, NULL_TREE, NULL_TREE);
    if (has_parm)
      TREE_USED (FUNCTION_FIRST_USER_PARM (fn)) = 1;
  }

  my_friendly_assert (TREE_CODE (fn) == FUNCTION_DECL, 20000408);

  DECL_ARTIFICIAL (fn) = 1;
  DECL_NOT_REALLY_EXTERN (fn) = 1;
  DECL_DECLARED_INLINE_P (fn) = 1;
  DECL_INLINE (fn) = 1;
  defer_fn (fn);
  
  return fn;
}

/* Given a FUNCTION_DECL FN and a chain LIST, skip as many elements of LIST
   as there are artificial parms in FN.  */

tree
skip_artificial_parms_for (tree fn, tree list)
{
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
    list = TREE_CHAIN (list);
  else
    return list;

  if (DECL_HAS_IN_CHARGE_PARM_P (fn))
    list = TREE_CHAIN (list);
  if (DECL_HAS_VTT_PARM_P (fn))
    list = TREE_CHAIN (list);
  return list;
}

#include "gt-cp-method.h"
