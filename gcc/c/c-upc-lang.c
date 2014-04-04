/* c-upc-lang.c: UPC language-specific functions.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

  tree init_func_ptr_type, init_func_addr;
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
#include "tm.h"
#include "tree.h"
#include "tree-upc.h"
#include "stringpool.h"
#include "tree-iterator.h"
#include "c/c-tree.h"
#include "c/c-objc-common.h"
#include "ggc.h"
#include "toplev.h"
#include "diagnostic.h"
#include "stor-layout.h"
#include "output.h"
#include "c-family/c-common.h"
#include "c-family/c-pragma.h"
#include "c-family/c-pretty-print.h"
#include "c-family/c-upc.h"
#include "c/c-upc-lang.h"
#include "c-family/c-upc-low.h"
#include "c-family/c-upc-pts.h"
#include "c-family/c-upc-rts-names.h"
#include "target.h"
#include "common/common-target.h"
#include "varasm.h"
#include "flags.h"
#include "opts.h"
#include "options.h"

static GTY (()) section *upc_init_array_section;

/* Create a static variable of type 'type'.
   This routine mimics the behavior of 'objc_create_temporary_var'
   with the change that it creates a static (file scoped) variable.  */
static tree
upc_create_static_var (tree type, const char *name)
{
  tree id = get_identifier (name);
  tree decl = build_decl (input_location, VAR_DECL, id, type);
  TREE_USED (decl) = 1;
  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  TREE_ADDRESSABLE (decl) = 0;
  DECL_PRESERVE_P (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = NULL;
  pushdecl_top_level (decl);
  return decl;
}

/* Return TRUE if DECL's size is zero,
   and DECL is a UPC shared array.  */
bool
upc_lang_layout_decl_p (tree decl, tree type)
{
  int need_to_size_shared_array_decl = 0;
  tree t = type;

  if (decl && DECL_SIZE (decl) == 0)
    {
      while (t != NULL && TREE_CODE (t) == ARRAY_TYPE
	     && TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	t = TREE_TYPE (t);

      if (t && TREE_CODE (t) == ARRAY_TYPE)
        {
	  tree tt = TREE_TYPE (t);
	  tree tt_size = TYPE_SIZE (tt);
          need_to_size_shared_array_decl =
	    upc_shared_type_p (tt)
	    && tt_size != NULL_TREE
            && !integer_zerop (tt_size);
        }
    }
  return need_to_size_shared_array_decl;
}

/* Given that TYPE describes a UPC shared array, and that DECL's size hasn't
   been calculated, calculate the size of the type and adjust the size
   attributes in DECL.  */

void
upc_lang_layout_decl (tree decl, tree type)
{
  tree t = type;
  gcc_checking_assert (upc_lang_layout_decl_p (decl, type));
  while (TREE_CODE (t) == ARRAY_TYPE
	 && TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    t = TREE_TYPE (t);

  {
    const tree elt_type = TREE_TYPE (t);
    const tree elt_size = TYPE_SIZE (elt_type);
    const tree block_factor = TYPE_HAS_UPC_BLOCK_FACTOR (elt_type)
      ? convert (bitsizetype, TYPE_UPC_BLOCK_FACTOR (elt_type)) : NULL;
    if (block_factor && integer_zerop (block_factor))
      {
	/* Allocate the entire UPC shared array on thread 0.  */
	if (TYPE_HAS_THREADS_FACTOR (type))
	  {
	    const tree n_threads =
	      convert (bitsizetype, upc_num_threads ());
	    DECL_SIZE (decl) = size_binop (MULT_EXPR, elt_size, n_threads);
	  }
	else
	  DECL_SIZE (decl) = TYPE_SIZE (type);
      }
    else
      {
	const tree t_size = TYPE_SIZE (type);
	const tree n_elem = size_binop (FLOOR_DIV_EXPR, t_size, elt_size);
	const tree n_threads = convert (bitsizetype, upc_num_threads ());
	if (TYPE_HAS_THREADS_FACTOR (type))
	  {
	    if (block_factor)
	      {
		const tree blk_size = convert (bitsizetype, block_factor);
		tree t1, t2;
		t1 = size_binop (CEIL_DIV_EXPR, n_elem, blk_size);
		t2 = size_binop (MULT_EXPR, t1, blk_size);
		DECL_SIZE (decl) = size_binop (MULT_EXPR, t2, elt_size);
	      }
	    else
	      DECL_SIZE (decl) = t_size;
	  }
	else
	  {
	    /* We want to allocate ceiling (N_ELEM / N_THREADS)
	       elements per thread, where N_ELEM is the total number of
	       elements in the array.  If the array is blocked,
	       then we allocate (ceiling (ceiling
		 (N_ELEM / BLOCK_FACTOR) / N_THREADS)
		 * block_factor) * N_ELEM_PER_THREAD.  */
	    tree n_elem_per_thread;
	    if (block_factor)
	      {
		tree block_count, blocks_per_thread;
		block_count = size_binop (CEIL_DIV_EXPR,
					  n_elem, block_factor);
		blocks_per_thread = size_binop (CEIL_DIV_EXPR,
						block_count, n_threads);
		n_elem_per_thread = size_binop (MULT_EXPR,
						blocks_per_thread,
						block_factor);
	      }
	    else
	      n_elem_per_thread = size_binop (CEIL_DIV_EXPR,
					      n_elem, n_threads);

	    /* In the special case of an array of size 1, we know that
	       we want a constant size no matter what N_THREADS is.  Make
	       the size a constant so that declarations of the form:
		 shared int x[1];
	       will work in a dynamic THREADS compilation environment.  */
	    if (integer_onep (n_elem))
	      DECL_SIZE (decl) = elt_size;
	    else
	      DECL_SIZE (decl) = size_binop (MULT_EXPR, n_elem_per_thread,
					     elt_size);
	  }
      }
    if (DECL_SIZE_UNIT (decl) == 0)
      DECL_SIZE_UNIT (decl)
	= fold_convert (sizetype,
			size_binop (CEIL_DIV_EXPR, DECL_SIZE (decl),
				    bitsize_unit_node));
  }
}

/* Initialize the UPC-specific parts of the compiler.  */

static void
upc_parse_init (void)
{
  upc_pts_init ();
  upc_genericize_init ();
}

/*
 * Build the internal representation of UPC's pointer-to-shared type.
 */
void
upc_pts_struct_init_type (void)
{
  tree fields = NULL_TREE;
  tree name = NULL_TREE;
  tree ref;
  enum machine_mode pts_mode;
  const location_t loc = UNKNOWN_LOCATION;
  struct c_struct_parse_info *null_struct_parse_info = NULL;
  int save_pedantic = pedantic;
  ref = start_struct (loc, RECORD_TYPE, name, &null_struct_parse_info);
  /* Ensure that shared pointers have twice the alignment of a pointer.  */
  TYPE_ALIGN (ref) = 2 * TYPE_ALIGN (ptr_type_node);
  TYPE_USER_ALIGN (ref) = 1;
  name = get_identifier ("vaddr");
  upc_vaddr_field_node = build_decl (loc, FIELD_DECL, name,
				     build_pointer_type (char_type_node));
  fields = chainon (fields, upc_vaddr_field_node);
  DECL_NONADDRESSABLE_P (upc_vaddr_field_node) = 0;
  DECL_INITIAL (upc_vaddr_field_node) = NULL_TREE;
  upc_thread_field_node =
    build_decl (loc, FIELD_DECL, get_identifier ("thread"),
		c_common_type_for_size (UPC_PTS_THREAD_SIZE, 1));
  fields = chainon (fields, upc_thread_field_node);
  if (!(UPC_PTS_THREAD_SIZE % 8))
    {
      DECL_NONADDRESSABLE_P (upc_thread_field_node) = 0;
      DECL_INITIAL (upc_thread_field_node) = NULL_TREE;
    }
  else
    {
      DECL_NONADDRESSABLE_P (upc_thread_field_node) = 1;
      DECL_INITIAL (upc_thread_field_node) = size_int (UPC_PTS_THREAD_SIZE);
    }
  upc_phase_field_node =
    build_decl (loc, FIELD_DECL, get_identifier ("phase"),
		c_common_type_for_size (UPC_PTS_PHASE_SIZE, 1));
  fields = chainon (fields, upc_phase_field_node);
  if (!(UPC_PTS_PHASE_SIZE % 8))
    {
      DECL_NONADDRESSABLE_P (upc_phase_field_node) = 0;
      DECL_INITIAL (upc_phase_field_node) = NULL_TREE;
    }
  else
    {
      DECL_NONADDRESSABLE_P (upc_phase_field_node) = 1;
      DECL_INITIAL (upc_phase_field_node) = size_int (UPC_PTS_PHASE_SIZE);
    }
#if !HAVE_UPC_PTS_VADDR_FIRST
  fields = nreverse (fields);
#endif
  /* avoid spurious complaints regarding the definition
   * `phase' and `thread'.  */
  pedantic = 0;
  upc_pts_rep_type_node = finish_struct (loc, ref, fields, NULL_TREE,
					 null_struct_parse_info);
  pedantic = save_pedantic;
  gcc_assert (TYPE_SIZE (upc_pts_rep_type_node));
  gcc_assert (tree_fits_uhwi_p (TYPE_SIZE (upc_pts_rep_type_node)));
  gcc_assert (tree_to_uhwi (TYPE_SIZE (upc_pts_rep_type_node))
	      == 2 * POINTER_SIZE);
  pts_mode = mode_for_size_tree (TYPE_SIZE (upc_pts_rep_type_node),
                                 MODE_INT, 0);
  gcc_assert (pts_mode != BLKmode);
  SET_TYPE_MODE(upc_pts_rep_type_node, pts_mode);
  record_builtin_type (RID_SHARED, "upc_shared_ptr_t",
		       upc_pts_rep_type_node);
}

/* Build a function that will be called by the UPC runtime
   to initialize UPC shared variables.  STMT_LIST is a
   list of initialization statements.  */

void
upc_build_init_func (tree stmt_list)
{
  tree init_func_id = get_identifier (UPC_INIT_DECLS_FUNC);
  struct c_declspecs *specs;
  struct c_typespec void_spec;
  struct c_declarator *init_func_decl;
  struct c_arg_info args;
  tree init_func, fn_body;
  tree init_func_ptr_type, init_func_addr;
  location_t loc = input_location;
  int decl_ok;
  memset (&void_spec, '\0', sizeof (struct c_typespec));
  void_spec.kind = ctsk_typedef;
  void_spec.spec = lookup_name (get_identifier ("void"));
  specs = declspecs_add_type (loc, build_null_declspecs (), void_spec);
  init_func_decl = build_id_declarator (init_func_id);
  init_func_decl->id_loc = loc;
  memset (&args, '\0', sizeof (struct c_arg_info));
  args.types = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  init_func_decl = build_function_declarator (&args, init_func_decl);
  decl_ok = start_function (specs, init_func_decl, NULL_TREE);
  gcc_assert (decl_ok);
  store_parm_decls ();
  init_func = current_function_decl;
  DECL_SOURCE_LOCATION (current_function_decl) = loc;
  TREE_PUBLIC (current_function_decl) = 0;
  TREE_USED (current_function_decl) = 1;
  fn_body = c_begin_compound_stmt (true);
  append_to_statement_list_force (stmt_list, &fn_body);
  fn_body = c_end_compound_stmt (loc, fn_body, true);
  add_stmt (fn_body);
  finish_function ();
  gcc_assert (DECL_RTL (init_func));
  mark_decl_referenced (init_func);
  DECL_PRESERVE_P (init_func) = 1;
  init_func_ptr_type = build_pointer_type (TREE_TYPE (init_func));
  init_func_addr = upc_create_static_var (init_func_ptr_type,
                                          "__upc_init_func_addr");
  DECL_INITIAL (init_func_addr) = build_unary_op (loc, ADDR_EXPR,
                                                  init_func, 0);
  DECL_SECTION_NAME (init_func_addr) = build_string (
                                    strlen (UPC_INIT_ARRAY_SECTION_NAME),
                                    UPC_INIT_ARRAY_SECTION_NAME);
}

/* Enable/Disable UPC keywords.
   If ENABLE is true, UPC keywords are enabled.
   If ENABLE is false, UPC keywords are removed from consideration.
   This routine is used by the implementation of
   "#pragma upc upc_code" and "#pragma upc c_code" respectively.  */

void
upc_toggle_keywords (bool enable)
{
  unsigned int i;
  tree id;
  for (i = 0; i < num_c_common_reswords; i++)
    {
      if (UPC_IS_KEYWORD (c_common_reswords[i].rid))
        {
	  id = get_identifier (c_common_reswords[i].word);
	  C_IS_RESERVED_WORD (id) = enable;
        }
    }
}

/* UPC language-specific initialization.  */

void
upc_lang_init (void)
{
  gcc_assert (flag_upc);
  if (!targetm_common.have_named_sections)
    {
      fatal_error ("UPC is not implemented on this target; "
		   "the target linker does not support separately "
		   "linked sections");
    }
  upc_parse_init ();
}

#include "gt-c-c-upc-lang.h"
