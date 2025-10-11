/* Lower Algol 68 modes to GCC trees.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "stringpool.h"
#include "tree.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "print-tree.h"

#include "a68.h"

static tree a68_lower_mode (MOID_T *m);

/*
 * Support routines and definitions.
 */

/* Build a stub TYPE_DECL for a given TYPE.

   This is used for TYPE_STUB_DECL so we can generate debug info for all our
   modes, so the TYPE_DECL has no name.  */

static void
build_stub_type_decl (tree type, tree context)
{
  if (TYPE_STUB_DECL (type))
    return;

  tree decl = build_decl (UNKNOWN_LOCATION,
			  TYPE_DECL,
			  NULL_TREE /* name */,
			  type);
  TREE_PUBLIC (decl) = 1;
  DECL_CONTEXT (decl) = context;
  TYPE_CONTEXT (type) = DECL_CONTEXT (decl);
  TYPE_NAME (type) = decl; /* Weird.  This is for typedefs! */
  TYPE_STUB_DECL (type) = decl;
}

/* Builds a record type whose name is NAME.  NFIELDS is the number of fields,
   provided as field ident/type pairs.

   This code is copied from the D front end.  */

static tree
make_struct_type (tree type, const char *name, int nfields, ...)
{
  tree fields = NULL_TREE;
  va_list ap;

  va_start (ap, nfields);

  for (int i = 0; i < nfields; i++)
    {
      tree ident = va_arg (ap, tree);
      tree type = va_arg (ap, tree);
      tree field = build_decl (BUILTINS_LOCATION, FIELD_DECL, ident, type);
      DECL_CHAIN (field) = fields;
      fields = field;
    }

  va_end (ap);

  if (type == NULL_TREE)
    type = make_node (RECORD_TYPE);
  finish_builtin_struct (type, name, fields, NULL_TREE);

  return type;
}

/* Iterate over all the field selectors FIELDS of a structure type and add them
   as fields to CONTEXT.  Returns the number of field selectors found.  */

static size_t
chain_struct_fields (PACK_T *fields, tree context)
{
  PACK_T *elem;
  size_t num_fields;

  for (num_fields = 0, elem = fields;
       elem != NO_PACK;
       FORWARD (elem), ++num_fields)
    {
      const char *field_name = TEXT (elem);
      MOID_T *field_mode = MOID (elem);
      tree field_type = a68_lower_mode (field_mode);

      /* Create the field declaration.
         The declaration is not a compiler-generated entity.
	 Do not ignore the declaration for symbolic debug purposes. */
      tree field_decl = build_decl ((NODE (field_mode)
				     ? a68_get_node_location (NODE (field_mode))
				     : UNKNOWN_LOCATION),
				    FIELD_DECL,
				    field_name ? get_identifier (field_name) : NULL_TREE,
				    field_type);
      DECL_ARTIFICIAL (field_decl) = 0;
      DECL_IGNORED_P (field_decl) = 0;

      /* If the mode of the field is not a ref then references to the field
	 cannot appear in a LHS of an assignment.  */
      TREE_READONLY (field_decl) = IS_REF (field_mode);

      /* Associate the tree field declaration and the front end node.  */
      DECL_LANG_SPECIFIC (field_decl) =
	(NODE (field_mode) ? a68_build_lang_decl (NODE (field_mode)) : NULL);

      /* Chain the field declaration in its containing context.  */
      DECL_FIELD_CONTEXT (field_decl) = context;
      TYPE_FIELDS (context) = chainon (TYPE_FIELDS (context), field_decl);
    }

  return num_fields;
}

/* If the union or struct type TYPE completes the type of any previous field
   declarations, lay them out now.  */

static void
finish_incomplete_fields (tree type)
{
  for (tree fwdref = TYPE_FORWARD_REFERENCES (type); fwdref != NULL_TREE;
       fwdref = TREE_CHAIN (fwdref))
    {
      tree field = TREE_VALUE (fwdref);
      tree struct_or_union_type = DECL_FIELD_CONTEXT (field);

      relayout_decl (field);
      bool type_complete = true;
      for (tree field = TYPE_FIELDS (struct_or_union_type);
	   field;
	   field = DECL_CHAIN (field))
	{
	  if (!COMPLETE_TYPE_P (TREE_TYPE (field)))
	    {
	      type_complete = false;
	      break;
	    }
	}

      if (type_complete)
	{
	  // XXX why this fires
	  //	  gcc_assert (!COMPLETE_TYPE_P (struct_or_union_type));
	  layout_type (struct_or_union_type);
	  /* Set the back-end type mode now that all fields have had their size
	     set.  */
	  compute_record_mode (struct_or_union_type);
	}
    };

  /* No more forward references to process.  */
  TYPE_FORWARD_REFERENCES (type) = NULL_TREE;
}

/*
 * Mode lowering routines.
 */

/* Lower a HIP mode to a GENERIC tree.
   HIP is the mode of NIL.  */

static tree
lower_hip_mode (MOID_T *m)
{
  static tree hip_type;

  if (hip_type == NULL)
    {
      hip_type = build_pointer_type (a68_void_type);
      TYPE_LANG_SPECIFIC (hip_type) = a68_build_lang_type (m);
      CTYPE (m) = hip_type;
    }

  return hip_type;
}

/* Lower a standard mode to a GENERIC tree.

   Note that this function only has to handle the standard modes that have not
   been resolved to some equivalent.  */

static tree
lower_standard_mode (MOID_T *m)
{
  tree type = NULL_TREE;

  if (m == M_VOID)
    type = a68_void_type;
  else if (m == M_BOOL)
    type = a68_bool_type;
  else if (m == M_CHAR)
    type = a68_char_type;
  else if (m == M_SHORT_SHORT_INT)
    type = a68_short_short_int_type;
  else if (m == M_SHORT_INT)
    type = a68_short_int_type;
  else if (m == M_INT)
    type = a68_int_type;
  else if (m == M_LONG_INT)
    type = a68_long_int_type;
  else if (m == M_LONG_LONG_INT)
    type = a68_long_long_int_type;
  else if (m == M_REAL)
    type = a68_real_type;
  else if (m == M_LONG_REAL)
    type = a68_long_real_type;
  else if (m == M_LONG_LONG_REAL)
    type = a68_long_long_real_type;
  else if (m == M_SHORT_SHORT_BITS)
    type = a68_short_short_bits_type;
  else if (m == M_SHORT_BITS)
    type = a68_short_bits_type;
  else if (m == M_BITS)
    type = a68_bits_type;
  else if (m == M_LONG_BITS)
    type = a68_long_bits_type;
  else if (m == M_LONG_LONG_BITS)
    type = a68_long_long_bits_type;
  else if (m == M_BYTES)
    type = a68_bytes_type;
  else if (m == M_LONG_BYTES)
    type = a68_long_bytes_type;
  else if (m == M_FILE)
    /* XXX for now this is a file descriptor.  */
    type = integer_type_node;
  else if (m == M_CHANNEL)
    /* XXX for now this is a channel descriptor.  */
    type = integer_type_node;
  else
    gcc_unreachable ();

  TYPE_LANG_SPECIFIC (type) = a68_build_lang_type (m);
  return type;
}

/* Lower a struct mode to a GENERIC tree.  */

static tree
lower_struct_mode (MOID_T *m)
{
  /* First make the GENERIC struct.  This is needed in case of
     self-references.  */
  tree struct_type = make_node (RECORD_TYPE);
  TYPE_NAME (struct_type) = get_identifier ("lalastruct%");
  TYPE_FIELDS (struct_type) = NULL_TREE;
  TYPE_CXX_ODR_P (struct_type) = 0;
  CTYPE (m) = struct_type;
  TYPE_LANG_SPECIFIC (struct_type) = a68_build_lang_type (m); /* XXX this will get overrided. */

  /* Add field declarations.  */
  chain_struct_fields (PACK (m), struct_type);

  /* Layout all fields.  */
  bool struct_type_complete = true;
  for (tree field = TYPE_FIELDS (struct_type); field; field = DECL_CHAIN (field))
    {
      tree basetype = TREE_TYPE (field);

      if (!COMPLETE_TYPE_P (basetype))
	{
	  tree field_type = TREE_TYPE (field);
	  tree forward_refs = tree_cons (NULL_TREE, field,
					 TYPE_FORWARD_REFERENCES (field_type));
	  TYPE_FORWARD_REFERENCES (struct_type) = forward_refs;

	  struct_type_complete = false;
	  continue;
	}

      layout_decl (field, 0);
      gcc_assert (DECL_SIZE (field) != NULL_TREE);
    }

  /* If all fields have complete types then we can layout the struct type now.
     Otherwise it will be done in finish_incomplete_types.  */
  if (struct_type_complete)
    {
      layout_type (struct_type);
      /* Set the back-end type mode now that all fields have had their size
	 set.  */
      compute_record_mode (struct_type);
    }

  /* Finish debugging output for this type.  */
  build_stub_type_decl (struct_type, NULL_TREE /* context */);
  rest_of_type_compilation (struct_type, TYPE_FILE_SCOPE_P (struct_type));
  rest_of_decl_compilation (TYPE_NAME (struct_type), 1 /* file scope p */, 0);
  A68_STRUCT_TYPE_P (struct_type) = 1;
  return struct_type;
}

/* Lower a ref mode to a GENERIC tree.
   REF AMODE lowers to a pointer.  */

static tree
lower_ref_mode (MOID_T *m)
{
  return build_pointer_type (a68_lower_mode (SUB (m)));
}

/* Lower a flex mode to a GENERIC tree.  */

static tree
lower_flex_mode (MOID_T *m)
{
  /* This is basically a qualifier of the parent REF.  */
  return a68_lower_mode (SUB (m));
}

/* Lower a proc mode to a GENERIC tree.  */

static tree
lower_proc_mode (MOID_T *m)
{
  tree fnargs = NULL_TREE;
  tree ret_type;

  /* We have to create the function type in advance because it can appear
     recursively as the type of arguments and/or of the return value.  We
     cannot use build_function_type, as it doesn't support recursive types.  */
  tree function_type = make_node (FUNCTION_TYPE);
  tree ptr_function_type = build_pointer_type (function_type);
  CTYPE (m) = ptr_function_type;

  /* Now add arguments and return value types.  */
  for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
    {
      tree arg_type = a68_lower_mode (MOID (p));
      fnargs = chainon (fnargs, build_tree_list (0, arg_type));
    }
  ret_type = a68_lower_mode (SUB (m));

  /* Complete the function type.  Note that there is some code duplication with
     build_function_type, which we cannot use, but such is life.  */
  TREE_TYPE (function_type) = ret_type; /* THIS */
  TYPE_ARG_TYPES (function_type) = fnargs;
  SET_TYPE_STRUCTURAL_EQUALITY (function_type);

  if (!COMPLETE_TYPE_P (function_type))
    layout_type (function_type);

  return ptr_function_type;
}

/* Lower an union mode to a GENERIC tree.

   overhead%     Characterizes the actual mode of the value.
   value%        GENERIC union.  */

static tree
lower_union_mode (MOID_T *m)
{
  // XXX make the union type QUAL_UNION_TYPE and relate the fields with the
  // overhead%.  This is necessary for DWARF.
  tree union_type = make_node (RECORD_TYPE);
  TYPE_NAME (union_type) = NULL_TREE;
  TYPE_FIELDS (union_type) = NULL_TREE;
  TYPE_CXX_ODR_P (union_type) = 0;
  CTYPE (m) = union_type;

  /* Then the GENERIC union.  */
  tree c_union_type = make_node (UNION_TYPE);
  TYPE_NAME (c_union_type) = NULL_TREE;
  TYPE_FIELDS (c_union_type) = NULL_TREE;
  TYPE_CXX_ODR_P (c_union_type) = 0; // XXX otherwise lto complains.  why.
  SET_TYPE_STRUCTURAL_EQUALITY (c_union_type);

  /* Add field declarations.  */
  chain_struct_fields (PACK (m), c_union_type);

  /* Layout all fields now the type is complete.  */
  bool c_union_type_complete = true;
  for (tree field = TYPE_FIELDS (c_union_type); field; field = DECL_CHAIN (field))
    {
      tree field_type = TREE_TYPE (field);

      if (!COMPLETE_TYPE_P (field_type))
	{
	  tree field_type = TREE_TYPE (field);
	  tree forward_refs = tree_cons (NULL_TREE, field,
					 TYPE_FORWARD_REFERENCES (field_type));
	  TYPE_FORWARD_REFERENCES (c_union_type) = forward_refs;

	  c_union_type_complete = false;
	  continue;
	}

      layout_decl (field, 0);
      gcc_assert (DECL_SIZE (field) != NULL_TREE);
    }

  /* If all fields have complete types then we can layout the c-union type now.
     Otherwise it will be done in finish_incomplete_types.  */
  if (c_union_type_complete)
    {
      layout_type (c_union_type);
      /* Set the back-end type mode now that all fields have had their size
	 set.  */
      compute_record_mode (c_union_type);
    }

  /* Finish debugging output for this type.  */
  build_stub_type_decl (c_union_type, NULL_TREE /* context */);
  rest_of_type_compilation (c_union_type, TYPE_FILE_SCOPE_P (c_union_type));
  rest_of_decl_compilation (TYPE_NAME (c_union_type), 1 /* file scope p */, 0);

  /* Now the type with the overhead.  */
  TYPE_NAME (union_type) = get_identifier ("union%");
  tree overhead_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
				    get_identifier ("overhead%"), sizetype);
  tree value_field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
				 get_identifier ("value%"), c_union_type);
  DECL_FIELD_CONTEXT (overhead_field) = union_type;
  DECL_FIELD_CONTEXT (value_field) = union_type;
  DECL_CHAIN (value_field) = NULL_TREE;
  DECL_CHAIN (overhead_field) = value_field;
  TYPE_FIELDS (union_type) = overhead_field;

  if (c_union_type_complete)
    {
      layout_type (union_type);
      /* Set the back-end type mode now that all fields have had their size
	 set.  */
      compute_record_mode (union_type);
    }
  else
    {
      tree forward_refs = tree_cons (NULL_TREE, value_field,
				     TYPE_FORWARD_REFERENCES (union_type));
      TYPE_FORWARD_REFERENCES (union_type) = forward_refs;
    }

  SET_TYPE_STRUCTURAL_EQUALITY (union_type);
  A68_UNION_TYPE_P (union_type) = 1;
  return union_type;
}

/* Return the type for an array descriptor triplet.  */

tree
a68_triplet_type (void)
{
  static tree triplet_type = NULL_TREE;
  if (triplet_type == NULL_TREE)
    {
      triplet_type = make_struct_type (NULL_TREE, "triplet%", 3,
				       get_identifier ("lb%"),
				       ssizetype,
				       get_identifier ("ub%"),
				       ssizetype,
				       get_identifier ("stride%"),
				       sizetype);
    }

  return triplet_type;
}

/* Return the lower bound field in an array descriptor triplet.  */

tree
a68_triplet_type_lower_bound (tree triplet)
{
  tree lb_field = TYPE_FIELDS (triplet);
  return lb_field;
}

/* Lower a row mode to a GENERIC tree.

   descriptor%
     triplets%     Value of ARRAY_TYPE with an entry per multiple dimension.
     {
       li%         Lower bound of dimension.
       ui%         Upper bound of dimension.
       di%         Stride of dimension in bytes.
     }
   elements%       Pointer to the elements.
   elements_size%  Size of elements% in bytes.
*/

static tree
lower_row_mode (MOID_T *m)
{
  int num_dimensions = DIM (m);
  tree triplet_type = a68_triplet_type ();
  tree triplets_type = build_array_type (triplet_type,
					 build_index_type (size_int (num_dimensions - 1)));
  tree element_type = a68_lower_mode (SUB (m));
  tree row_type = make_struct_type (NULL_TREE, "row%", 3,
				    get_identifier ("triplets%"),
				    triplets_type,
				    get_identifier ("elements%"),
				    build_pointer_type (element_type),
				    get_identifier ("elements_size%"),
				    sizetype);
  layout_type (row_type);
  A68_ROW_TYPE_P (row_type) = 1;
  return row_type;
}

/* Given a row type, return the type of the pointer to its elements.  */

tree
a68_row_elements_pointer_type (tree type)
{
  gcc_assert (A68_ROW_TYPE_P (type));
  /* elements% is the second field.  */
  return TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type)));
}

/* Given a row type, return the type of its elements.  */

tree
a68_row_elements_type (tree type)
{
  return TREE_TYPE (a68_row_elements_pointer_type (type));
}

/* Lower a ROWS mode to a GENERIC tree.

   dim%         Number of dimensions.
   triplets%    Pointer to triplets.

   Values of this mode are passed to the operators UPB, LWB and ELEMS, which
   need only descriptor information.  There is no need to store any multiple
   elements.  */

static tree
lower_rows_mode (MOID_T *m ATTRIBUTE_UNUSED)
{
  static tree rows_type = NULL_TREE;

  if (rows_type == NULL_TREE)
    {
      rows_type = make_struct_type (NULL_TREE, "rows%", 2,
				    get_identifier ("dim%"),
				    sizetype,
				    get_identifier ("triplets%"),
				    build_pointer_type (a68_triplet_type ()));
      A68_ROWS_TYPE_P (rows_type) = 1;
    }
  return rows_type;
}

/* Lower modes in a series.  This is used as the mode of the mode yielded by an
   enclosed clause that yields a series of united rows, for M_ROWS.  */

static tree
lower_series (MOID_T *m)
{
  for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
    {
      if (IS (MOID (p), SERIES_MODE) || IS (MOID (p), STOWED_MODE))
	lower_series (MOID (p));
      else
	(void) a68_lower_mode (MOID (p));
    }

  return lower_rows_mode (NO_MOID);
}

/* Lower a mode to a GENERIC tree.  */

static tree
a68_lower_mode (MOID_T *m)
{
  tree type = NULL_TREE;

  /* If the given mode has already been lowered, return the corresponding
     tree.  */
  if (CTYPE (m) != NULL)
    return CTYPE (m);

  if (EQUIVALENT (m) != NO_MOID && EQUIVALENT (m) != m)
    /* This covers INDICANTs and standard MOIDS having an equivalent mode.  */
    type = a68_lower_mode (EQUIVALENT (m));
  else if (m == M_VOID)
    type = a68_void_type;
  else if (m == M_HIP)
    type = lower_hip_mode (m);
  else if (IS (m, STANDARD))
    type = lower_standard_mode (m);
  else if (IS_REF (m))
    type = lower_ref_mode (m);
  else if (IS_FLEX (m))
    type = lower_flex_mode (m);
  else if (IS (m, PROC_SYMBOL))
    type = lower_proc_mode (m);
  else if (IS_STRUCT (m))
    type = lower_struct_mode (m);
  else if (IS_ROW (m))
    type = lower_row_mode (m);
  else if (IS_UNION (m))
    type = lower_union_mode (m);
  else if (m == M_SIMPLOUT || m == M_SIMPLIN)
    type = a68_void_type;
  else if (IS (m, ROWS_SYMBOL))
    /* ROWS is a mode that means "any row mode".  */
    type = lower_rows_mode (m);
  else if (m == M_VACUUM)
    /* This is a mode that should not survive the parser.  */
    type = a68_void_type;
  else if (IS (m, SERIES_MODE) || IS (m, STOWED_MODE))
    {
      /* When dealing with operators the parser creates some modes that leak
	 SERIES and STOWED "proto-modes" in them, such as for example:

	 UNION ((INT, INT), INT, PROC [] CHAR)

	 These are not really real Algol 68 modes and are useless by
	 themselves, so when we find them, we traverse them (they ultimately
	 contain valid modes that may show up in other contexts and that
	 require being lowered) and just report them as VOID.  */
      type = lower_series (m);
    }
  else
    {
      fatal_error (NODE (m) ? a68_get_node_location (NODE (m)) : UNKNOWN_LOCATION,
		   "Cannot lower mode %s",
		   a68_moid_to_string (m, MOID_ERROR_WIDTH, NODE (m)));
    }

  /* Associate the created tree node with the mode, and vice-versa.  */
  gcc_assert (type != NULL_TREE);
  TYPE_LANG_SPECIFIC (type) = a68_build_lang_type (m);
  A68_TYPE_HAS_ROWS_P (type) = HAS_ROWS (m);
  if (CTYPE (m) == NULL_TREE)
    CTYPE (m) = type;
  //  printf ("DONE LOWERING %s\n", a68_moid_to_string (m, MOID_ERROR_WIDTH, NODE (m)));
  return type;
}

/* Lower MOIDs to GENERIC trees.  */

void
a68_lower_moids (MOID_T *mode)
{
  /* First pass: all modes but refs.  */
  for (MOID_T *m = mode; m != NO_MOID; FORWARD (m))
    (void) a68_lower_mode (m);

  /* Try to layout all incomplete types.  This is a two-passes process.  */

  for (MOID_T *m = mode; m != NO_MOID; FORWARD (m))
    {
      if (IS_STRUCT (m))
	{
	  tree struct_type = CTYPE (m);
	  finish_incomplete_fields (struct_type);
	}
      else if (IS_UNION (m))
	{
	  tree union_type = CTYPE (m);
	  tree c_union_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (union_type)));
	  finish_incomplete_fields (c_union_type);
	  finish_incomplete_fields (union_type);
	}
    }

  for (MOID_T *m = mode; m != NO_MOID; FORWARD (m))
    {
      if (!COMPLETE_TYPE_P (CTYPE (m)))
	{
	  if (IS_STRUCT (m))
	    {
	      tree struct_type = CTYPE (m);
	      layout_type (struct_type);
	      compute_record_mode (struct_type);
	    }
	  else if (IS_UNION (m))
	    {
	      tree union_type = CTYPE (m);
	      tree c_union_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (union_type)));

	      if (!COMPLETE_TYPE_P (c_union_type))
		{
		  layout_type (c_union_type);
		  compute_record_mode (c_union_type);
		}

	      layout_type (union_type);
	      compute_record_mode (union_type);
	    }
	  else
	    layout_type (CTYPE (m));
	}
    }

  /* Sanity check.  */
  for (MOID_T *m = mode; m != NO_MOID; FORWARD (m))
    {
      gcc_assert (COMPLETE_TYPE_P (CTYPE (m)));
      if (IS_UNION (m))
	{
	  tree union_type = CTYPE (m);
	  tree c_union_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (union_type)));
	  gcc_assert (COMPLETE_TYPE_P (c_union_type));
	}
    }
}
