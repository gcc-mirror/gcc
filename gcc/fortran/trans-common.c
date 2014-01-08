/* Common block and equivalence list handling
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
   Contributed by Canqun Yang <canqun@nudt.edu.cn>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */     

/* The core algorithm is based on Andy Vaught's g95 tree.  Also the
   way to build UNION_TYPE is borrowed from Richard Henderson.
 
   Transform common blocks.  An integral part of this is processing
   equivalence variables.  Equivalenced variables that are not in a
   common block end up in a private block of their own.

   Each common block or local equivalence list is declared as a union.
   Variables within the block are represented as a field within the
   block with the proper offset. 
 
   So if two variables are equivalenced, they just point to a common
   area in memory.
 
   Mathematically, laying out an equivalence block is equivalent to
   solving a linear system of equations.  The matrix is usually a
   sparse matrix in which each row contains all zero elements except
   for a +1 and a -1, a sort of a generalized Vandermonde matrix.  The
   matrix is usually block diagonal.  The system can be
   overdetermined, underdetermined or have a unique solution.  If the
   system is inconsistent, the program is not standard conforming.
   The solution vector is integral, since all of the pivots are +1 or -1.
 
   How we lay out an equivalence block is a little less complicated.
   In an equivalence list with n elements, there are n-1 conditions to
   be satisfied.  The conditions partition the variables into what we
   will call segments.  If A and B are equivalenced then A and B are
   in the same segment.  If B and C are equivalenced as well, then A,
   B and C are in a segment and so on.  Each segment is a block of
   memory that has one or more variables equivalenced in some way.  A
   common block is made up of a series of segments that are joined one
   after the other.  In the linear system, a segment is a block
   diagonal.
 
   To lay out a segment we first start with some variable and
   determine its length.  The first variable is assumed to start at
   offset one and extends to however long it is.  We then traverse the
   list of equivalences to find an unused condition that involves at
   least one of the variables currently in the segment.
 
   Each equivalence condition amounts to the condition B+b=C+c where B
   and C are the offsets of the B and C variables, and b and c are
   constants which are nonzero for array elements, substrings or
   structure components.  So for
 
     EQUIVALENCE(B(2), C(3))
   we have
     B + 2*size of B's elements = C + 3*size of C's elements.
 
   If B and C are known we check to see if the condition already
   holds.  If B is known we can solve for C.  Since we know the length
   of C, we can see if the minimum and maximum extents of the segment
   are affected.  Eventually, we make a full pass through the
   equivalence list without finding any new conditions and the segment
   is fully specified.
 
   At this point, the segment is added to the current common block.
   Since we know the minimum extent of the segment, everything in the
   segment is translated to its position in the common block.  The
   usual case here is that there are no equivalence statements and the
   common block is series of segments with one variable each, which is
   a diagonal matrix in the matrix formulation.
 
   Each segment is described by a chain of segment_info structures.  Each
   segment_info structure describes the extents of a single variable within
   the segment.  This list is maintained in the order the elements are
   positioned within the segment.  If two elements have the same starting
   offset the smaller will come first.  If they also have the same size their
   ordering is undefined. 
   
   Once all common blocks have been created, the list of equivalences
   is examined for still-unused equivalence conditions.  We create a
   block for each merged equivalence list.  */

#include <map>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"
#include "target-memory.h"


/* Holds a single variable in an equivalence set.  */
typedef struct segment_info
{
  gfc_symbol *sym;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT length;
  /* This will contain the field type until the field is created.  */
  tree field;
  struct segment_info *next;
} segment_info;

static segment_info * current_segment;

/* Store decl of all common blocks in this translation unit; the first
   tree is the identifier.  */
static std::map<tree, tree> gfc_map_of_all_commons;


/* Make a segment_info based on a symbol.  */

static segment_info *
get_segment_info (gfc_symbol * sym, HOST_WIDE_INT offset)
{
  segment_info *s;

  /* Make sure we've got the character length.  */
  if (sym->ts.type == BT_CHARACTER)
    gfc_conv_const_charlen (sym->ts.u.cl);

  /* Create the segment_info and fill it in.  */
  s = XCNEW (segment_info);
  s->sym = sym;
  /* We will use this type when building the segment aggregate type.  */
  s->field = gfc_sym_type (sym);
  s->length = int_size_in_bytes (s->field);
  s->offset = offset;

  return s;
}


/* Add a copy of a segment list to the namespace.  This is specifically for
   equivalence segments, so that dependency checking can be done on
   equivalence group members.  */

static void
copy_equiv_list_to_ns (segment_info *c)
{
  segment_info *f;
  gfc_equiv_info *s;
  gfc_equiv_list *l;

  l = XCNEW (gfc_equiv_list);

  l->next = c->sym->ns->equiv_lists;
  c->sym->ns->equiv_lists = l;

  for (f = c; f; f = f->next)
    {
      s = XCNEW (gfc_equiv_info);
      s->next = l->equiv;
      l->equiv = s;
      s->sym = f->sym;
      s->offset = f->offset;
      s->length = f->length;
    }
}


/* Add combine segment V and segment LIST.  */

static segment_info *
add_segments (segment_info *list, segment_info *v)
{
  segment_info *s;
  segment_info *p;
  segment_info *next;

  p = NULL;
  s = list;

  while (v)
    {
      /* Find the location of the new element.  */
      while (s)
	{
	  if (v->offset < s->offset)
	    break;
	  if (v->offset == s->offset
	      && v->length <= s->length)
	    break;

	  p = s;
	  s = s->next;
	}

      /* Insert the new element in between p and s.  */
      next = v->next;
      v->next = s;
      if (p == NULL)
	list = v;
      else
	p->next = v;

      p = v;
      v = next;
    }

  return list;
}


/* Construct mangled common block name from symbol name.  */

/* We need the bind(c) flag to tell us how/if we should mangle the symbol
   name.  There are few calls to this function, so few places that this
   would need to be added.  At the moment, there is only one call, in
   build_common_decl().  We can't attempt to look up the common block
   because we may be building it for the first time and therefore, it won't
   be in the common_root.  We also need the binding label, if it's bind(c).
   Therefore, send in the pointer to the common block, so whatever info we
   have so far can be used.  All of the necessary info should be available
   in the gfc_common_head by now, so it should be accurate to test the
   isBindC flag and use the binding label given if it is bind(c).

   We may NOT know yet if it's bind(c) or not, but we can try at least.
   Will have to figure out what to do later if it's labeled bind(c)
   after this is called.  */

static tree
gfc_sym_mangled_common_id (gfc_common_head *com)
{
  int has_underscore;
  char mangled_name[GFC_MAX_MANGLED_SYMBOL_LEN + 1];
  char name[GFC_MAX_SYMBOL_LEN + 1];

  /* Get the name out of the common block pointer.  */
  strcpy (name, com->name);

  /* If we're suppose to do a bind(c).  */
  if (com->is_bind_c == 1 && com->binding_label)
    return get_identifier (com->binding_label);

  if (strcmp (name, BLANK_COMMON_NAME) == 0)
    return get_identifier (name);

  if (gfc_option.flag_underscoring)
    {
      has_underscore = strchr (name, '_') != 0;
      if (gfc_option.flag_second_underscore && has_underscore)
        snprintf (mangled_name, sizeof mangled_name, "%s__", name);
      else
        snprintf (mangled_name, sizeof mangled_name, "%s_", name);

      return get_identifier (mangled_name);
    }
  else
    return get_identifier (name);
}


/* Build a field declaration for a common variable or a local equivalence
   object.  */

static void
build_field (segment_info *h, tree union_type, record_layout_info rli)
{
  tree field;
  tree name;
  HOST_WIDE_INT offset = h->offset;
  unsigned HOST_WIDE_INT desired_align, known_align;

  name = get_identifier (h->sym->name);
  field = build_decl (h->sym->declared_at.lb->location,
		      FIELD_DECL, name, h->field);
  known_align = (offset & -offset) * BITS_PER_UNIT;
  if (known_align == 0 || known_align > BIGGEST_ALIGNMENT)
    known_align = BIGGEST_ALIGNMENT;

  desired_align = update_alignment_for_field (rli, field, known_align);
  if (desired_align > known_align)
    DECL_PACKED (field) = 1;

  DECL_FIELD_CONTEXT (field) = union_type;
  DECL_FIELD_OFFSET (field) = size_int (offset);
  DECL_FIELD_BIT_OFFSET (field) = bitsize_zero_node;
  SET_DECL_OFFSET_ALIGN (field, known_align);

  rli->offset = size_binop (MAX_EXPR, rli->offset,
                            size_binop (PLUS_EXPR,
                                        DECL_FIELD_OFFSET (field),
                                        DECL_SIZE_UNIT (field)));
  /* If this field is assigned to a label, we create another two variables.
     One will hold the address of target label or format label. The other will
     hold the length of format label string.  */
  if (h->sym->attr.assign)
    {
      tree len;
      tree addr;

      gfc_allocate_lang_decl (field);
      GFC_DECL_ASSIGN (field) = 1;
      len = gfc_create_var_np (gfc_charlen_type_node,h->sym->name);
      addr = gfc_create_var_np (pvoid_type_node, h->sym->name);
      TREE_STATIC (len) = 1;
      TREE_STATIC (addr) = 1;
      DECL_INITIAL (len) = build_int_cst (gfc_charlen_type_node, -2);
      gfc_set_decl_location (len, &h->sym->declared_at);
      gfc_set_decl_location (addr, &h->sym->declared_at);
      GFC_DECL_STRING_LEN (field) = pushdecl_top_level (len);
      GFC_DECL_ASSIGN_ADDR (field) = pushdecl_top_level (addr);
    }

  /* If this field is volatile, mark it.  */
  if (h->sym->attr.volatile_)
    {
      tree new_type;
      TREE_THIS_VOLATILE (field) = 1;
      TREE_SIDE_EFFECTS (field) = 1;
      new_type = build_qualified_type (TREE_TYPE (field), TYPE_QUAL_VOLATILE);
      TREE_TYPE (field) = new_type;
    }

  h->field = field;
}


/* Get storage for local equivalence.  */

static tree
build_equiv_decl (tree union_type, bool is_init, bool is_saved)
{
  tree decl;
  char name[15];
  static int serial = 0;

  if (is_init)
    {
      decl = gfc_create_var (union_type, "equiv");
      TREE_STATIC (decl) = 1;
      GFC_DECL_COMMON_OR_EQUIV (decl) = 1;
      return decl;
    }

  snprintf (name, sizeof (name), "equiv.%d", serial++);
  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (name), union_type);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;

  if (!gfc_can_put_var_on_stack (DECL_SIZE_UNIT (decl))
      || is_saved)
    TREE_STATIC (decl) = 1;

  TREE_ADDRESSABLE (decl) = 1;
  TREE_USED (decl) = 1;
  GFC_DECL_COMMON_OR_EQUIV (decl) = 1;

  /* The source location has been lost, and doesn't really matter.
     We need to set it to something though.  */
  gfc_set_decl_location (decl, &gfc_current_locus);

  gfc_add_decl_to_function (decl);

  return decl;
}


/* Get storage for common block.  */

static tree
build_common_decl (gfc_common_head *com, tree union_type, bool is_init)
{
  tree decl, identifier;

  identifier = gfc_sym_mangled_common_id (com);
  decl = gfc_map_of_all_commons.count(identifier)
	 ? gfc_map_of_all_commons[identifier] : NULL_TREE;

  /* Update the size of this common block as needed.  */
  if (decl != NULL_TREE)
    {
      tree size = TYPE_SIZE_UNIT (union_type);

      /* Named common blocks of the same name shall be of the same size
	 in all scoping units of a program in which they appear, but
	 blank common blocks may be of different sizes.  */
      if (!tree_int_cst_equal (DECL_SIZE_UNIT (decl), size)
	  && strcmp (com->name, BLANK_COMMON_NAME))
	gfc_warning ("Named COMMON block '%s' at %L shall be of the "
		     "same size as elsewhere (%lu vs %lu bytes)", com->name,
		     &com->where,
		     (unsigned long) TREE_INT_CST_LOW (size),
		     (unsigned long) TREE_INT_CST_LOW (DECL_SIZE_UNIT (decl)));

      if (tree_int_cst_lt (DECL_SIZE_UNIT (decl), size))
	{
	  DECL_SIZE (decl) = TYPE_SIZE (union_type);
	  DECL_SIZE_UNIT (decl) = size;
	  DECL_MODE (decl) = TYPE_MODE (union_type);
	  TREE_TYPE (decl) = union_type;
	  layout_decl (decl, 0);
	}
     }

  /* If this common block has been declared in a previous program unit,
     and either it is already initialized or there is no new initialization
     for it, just return.  */
  if ((decl != NULL_TREE) && (!is_init || DECL_INITIAL (decl)))
    return decl;

  /* If there is no backend_decl for the common block, build it.  */
  if (decl == NULL_TREE)
    {
      if (com->is_bind_c == 1 && com->binding_label)
	decl = build_decl (input_location, VAR_DECL, identifier, union_type);
      else
	{
	  decl = build_decl (input_location, VAR_DECL, get_identifier (com->name),
			     union_type);
	  gfc_set_decl_assembler_name (decl, identifier);
	}

      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      if (!com->is_bind_c)
	DECL_ALIGN (decl) = BIGGEST_ALIGNMENT;
      else
        {
	  /* Do not set the alignment for bind(c) common blocks to
	     BIGGEST_ALIGNMENT because that won't match what C does.  Also,
	     for common blocks with one element, the alignment must be
	     that of the field within the common block in order to match
	     what C will do.  */
	  tree field = NULL_TREE;
	  field = TYPE_FIELDS (TREE_TYPE (decl));
	  if (DECL_CHAIN (field) == NULL_TREE)
	    DECL_ALIGN (decl) = TYPE_ALIGN (TREE_TYPE (field));
	}
      DECL_USER_ALIGN (decl) = 0;
      GFC_DECL_COMMON_OR_EQUIV (decl) = 1;

      gfc_set_decl_location (decl, &com->where);

      if (com->threadprivate)
	DECL_TLS_MODEL (decl) = decl_default_tls_model (decl);

      /* Place the back end declaration for this common block in
         GLOBAL_BINDING_LEVEL.  */
      gfc_map_of_all_commons[identifier] = pushdecl_top_level (decl);
    }

  /* Has no initial values.  */
  if (!is_init)
    {
      DECL_INITIAL (decl) = NULL_TREE;
      DECL_COMMON (decl) = 1;
      DECL_DEFER_OUTPUT (decl) = 1;
    }
  else
    {
      DECL_INITIAL (decl) = error_mark_node;
      DECL_COMMON (decl) = 0;
      DECL_DEFER_OUTPUT (decl) = 0;
    }
  return decl;
}


/* Return a field that is the size of the union, if an equivalence has
   overlapping initializers.  Merge the initializers into a single
   initializer for this new field, then free the old ones.  */ 

static tree
get_init_field (segment_info *head, tree union_type, tree *field_init,
		record_layout_info rli)
{
  segment_info *s;
  HOST_WIDE_INT length = 0;
  HOST_WIDE_INT offset = 0;
  unsigned HOST_WIDE_INT known_align, desired_align;
  bool overlap = false;
  tree tmp, field;
  tree init;
  unsigned char *data, *chk;
  vec<constructor_elt, va_gc> *v = NULL;

  tree type = unsigned_char_type_node;
  int i;

  /* Obtain the size of the union and check if there are any overlapping
     initializers.  */
  for (s = head; s; s = s->next)
    {
      HOST_WIDE_INT slen = s->offset + s->length;
      if (s->sym->value)
	{
	  if (s->offset < offset)
            overlap = true;
	  offset = slen;
	}
      length = length < slen ? slen : length;
    }

  if (!overlap)
    return NULL_TREE;

  /* Now absorb all the initializer data into a single vector,
     whilst checking for overlapping, unequal values.  */
  data = XCNEWVEC (unsigned char, (size_t)length);
  chk = XCNEWVEC (unsigned char, (size_t)length);

  /* TODO - change this when default initialization is implemented.  */
  memset (data, '\0', (size_t)length);
  memset (chk, '\0', (size_t)length);
  for (s = head; s; s = s->next)
    if (s->sym->value)
      gfc_merge_initializers (s->sym->ts, s->sym->value,
			      &data[s->offset],
			      &chk[s->offset],
			     (size_t)s->length);
  
  for (i = 0; i < length; i++)
    CONSTRUCTOR_APPEND_ELT (v, NULL, build_int_cst (type, data[i]));

  free (data);
  free (chk);

  /* Build a char[length] array to hold the initializers.  Much of what
     follows is borrowed from build_field, above.  */

  tmp = build_int_cst (gfc_array_index_type, length - 1);
  tmp = build_range_type (gfc_array_index_type,
			  gfc_index_zero_node, tmp);
  tmp = build_array_type (type, tmp);
  field = build_decl (gfc_current_locus.lb->location,
		      FIELD_DECL, NULL_TREE, tmp);

  known_align = BIGGEST_ALIGNMENT;

  desired_align = update_alignment_for_field (rli, field, known_align);
  if (desired_align > known_align)
    DECL_PACKED (field) = 1;

  DECL_FIELD_CONTEXT (field) = union_type;
  DECL_FIELD_OFFSET (field) = size_int (0);
  DECL_FIELD_BIT_OFFSET (field) = bitsize_zero_node;
  SET_DECL_OFFSET_ALIGN (field, known_align);

  rli->offset = size_binop (MAX_EXPR, rli->offset,
                            size_binop (PLUS_EXPR,
                                        DECL_FIELD_OFFSET (field),
                                        DECL_SIZE_UNIT (field)));

  init = build_constructor (TREE_TYPE (field), v);
  TREE_CONSTANT (init) = 1;

  *field_init = init;

  for (s = head; s; s = s->next)
    {
      if (s->sym->value == NULL)
	continue;

      gfc_free_expr (s->sym->value);
      s->sym->value = NULL;
    }

  return field;
}


/* Declare memory for the common block or local equivalence, and create
   backend declarations for all of the elements.  */

static void
create_common (gfc_common_head *com, segment_info *head, bool saw_equiv)
{
  segment_info *s, *next_s;
  tree union_type;
  tree *field_link;
  tree field;
  tree field_init = NULL_TREE;
  record_layout_info rli;
  tree decl;
  bool is_init = false;
  bool is_saved = false;

  /* Declare the variables inside the common block.
     If the current common block contains any equivalence object, then
     make a UNION_TYPE node, otherwise RECORD_TYPE. This will let the
     alias analyzer work well when there is no address overlapping for
     common variables in the current common block.  */
  if (saw_equiv)
    union_type = make_node (UNION_TYPE);
  else
    union_type = make_node (RECORD_TYPE);

  rli = start_record_layout (union_type);
  field_link = &TYPE_FIELDS (union_type);

  /* Check for overlapping initializers and replace them with a single,
     artificial field that contains all the data.  */
  if (saw_equiv)
    field = get_init_field (head, union_type, &field_init, rli);
  else
    field = NULL_TREE;

  if (field != NULL_TREE)
    {
      is_init = true;
      *field_link = field;
      field_link = &DECL_CHAIN (field);
    }

  for (s = head; s; s = s->next)
    {
      build_field (s, union_type, rli);

      /* Link the field into the type.  */
      *field_link = s->field;
      field_link = &DECL_CHAIN (s->field);

      /* Has initial value.  */
      if (s->sym->value)
        is_init = true;

      /* Has SAVE attribute.  */
      if (s->sym->attr.save)
        is_saved = true;
    }

  finish_record_layout (rli, true);

  if (com)
    decl = build_common_decl (com, union_type, is_init);
  else
    decl = build_equiv_decl (union_type, is_init, is_saved);

  if (is_init)
    {
      tree ctor, tmp;
      vec<constructor_elt, va_gc> *v = NULL;

      if (field != NULL_TREE && field_init != NULL_TREE)
	CONSTRUCTOR_APPEND_ELT (v, field, field_init);
      else
	for (s = head; s; s = s->next)
	  {
	    if (s->sym->value)
	      {
		/* Add the initializer for this field.  */
		tmp = gfc_conv_initializer (s->sym->value, &s->sym->ts,
					    TREE_TYPE (s->field),
					    s->sym->attr.dimension,
					    s->sym->attr.pointer
					    || s->sym->attr.allocatable, false);

		CONSTRUCTOR_APPEND_ELT (v, s->field, tmp);
	      }
	  }

      gcc_assert (!v->is_empty ());
      ctor = build_constructor (union_type, v);
      TREE_CONSTANT (ctor) = 1;
      TREE_STATIC (ctor) = 1;
      DECL_INITIAL (decl) = ctor;

#ifdef ENABLE_CHECKING
      {
	tree field, value;
	unsigned HOST_WIDE_INT idx;
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), idx, field, value)
	  gcc_assert (TREE_CODE (field) == FIELD_DECL);
      }
#endif
    }

  /* Build component reference for each variable.  */
  for (s = head; s; s = next_s)
    {
      tree var_decl;

      var_decl = build_decl (s->sym->declared_at.lb->location,
			     VAR_DECL, DECL_NAME (s->field),
			     TREE_TYPE (s->field));
      TREE_STATIC (var_decl) = TREE_STATIC (decl);
      /* Mark the variable as used in order to avoid warnings about
	 unused variables.  */
      TREE_USED (var_decl) = 1;
      if (s->sym->attr.use_assoc)
	DECL_IGNORED_P (var_decl) = 1;
      if (s->sym->attr.target)
	TREE_ADDRESSABLE (var_decl) = 1;
      /* Fake variables are not visible from other translation units. */
      TREE_PUBLIC (var_decl) = 0;

      /* To preserve identifier names in COMMON, chain to procedure
         scope unless at top level in a module definition.  */
      if (com
          && s->sym->ns->proc_name
          && s->sym->ns->proc_name->attr.flavor == FL_MODULE)
	var_decl = pushdecl_top_level (var_decl);
      else
	gfc_add_decl_to_function (var_decl);

      SET_DECL_VALUE_EXPR (var_decl,
			   fold_build3_loc (input_location, COMPONENT_REF,
					    TREE_TYPE (s->field),
					    decl, s->field, NULL_TREE));
      DECL_HAS_VALUE_EXPR_P (var_decl) = 1;
      GFC_DECL_COMMON_OR_EQUIV (var_decl) = 1;

      if (s->sym->attr.assign)
	{
	  gfc_allocate_lang_decl (var_decl);
	  GFC_DECL_ASSIGN (var_decl) = 1;
	  GFC_DECL_STRING_LEN (var_decl) = GFC_DECL_STRING_LEN (s->field);
	  GFC_DECL_ASSIGN_ADDR (var_decl) = GFC_DECL_ASSIGN_ADDR (s->field);
	}

      s->sym->backend_decl = var_decl;

      next_s = s->next;
      free (s);
    }
}


/* Given a symbol, find it in the current segment list. Returns NULL if
   not found.  */

static segment_info *
find_segment_info (gfc_symbol *symbol)
{
  segment_info *n;

  for (n = current_segment; n; n = n->next)
    {
      if (n->sym == symbol)
	return n;
    }

  return NULL;
}


/* Given an expression node, make sure it is a constant integer and return
   the mpz_t value.  */

static mpz_t *
get_mpz (gfc_expr *e)
{

  if (e->expr_type != EXPR_CONSTANT)
    gfc_internal_error ("get_mpz(): Not an integer constant");

  return &e->value.integer;
}


/* Given an array specification and an array reference, figure out the
   array element number (zero based). Bounds and elements are guaranteed
   to be constants.  If something goes wrong we generate an error and
   return zero.  */
 
static HOST_WIDE_INT
element_number (gfc_array_ref *ar)
{
  mpz_t multiplier, offset, extent, n;
  gfc_array_spec *as;
  HOST_WIDE_INT i, rank;

  as = ar->as;
  rank = as->rank;
  mpz_init_set_ui (multiplier, 1);
  mpz_init_set_ui (offset, 0);
  mpz_init (extent);
  mpz_init (n);

  for (i = 0; i < rank; i++)
    { 
      if (ar->dimen_type[i] != DIMEN_ELEMENT)
        gfc_internal_error ("element_number(): Bad dimension type");

      mpz_sub (n, *get_mpz (ar->start[i]), *get_mpz (as->lower[i]));
 
      mpz_mul (n, n, multiplier);
      mpz_add (offset, offset, n);
 
      mpz_sub (extent, *get_mpz (as->upper[i]), *get_mpz (as->lower[i]));
      mpz_add_ui (extent, extent, 1);
 
      if (mpz_sgn (extent) < 0)
        mpz_set_ui (extent, 0);
 
      mpz_mul (multiplier, multiplier, extent);
    } 
 
  i = mpz_get_ui (offset);
 
  mpz_clear (multiplier);
  mpz_clear (offset);
  mpz_clear (extent);
  mpz_clear (n);
 
  return i;
}


/* Given a single element of an equivalence list, figure out the offset
   from the base symbol.  For simple variables or full arrays, this is
   simply zero.  For an array element we have to calculate the array
   element number and multiply by the element size. For a substring we
   have to calculate the further reference.  */

static HOST_WIDE_INT
calculate_offset (gfc_expr *e)
{
  HOST_WIDE_INT n, element_size, offset;
  gfc_typespec *element_type;
  gfc_ref *reference;

  offset = 0;
  element_type = &e->symtree->n.sym->ts;

  for (reference = e->ref; reference; reference = reference->next)
    switch (reference->type)
      {
      case REF_ARRAY:
        switch (reference->u.ar.type)
          {
          case AR_FULL:
	    break;

          case AR_ELEMENT:
	    n = element_number (&reference->u.ar);
	    if (element_type->type == BT_CHARACTER)
	      gfc_conv_const_charlen (element_type->u.cl);
	    element_size =
              int_size_in_bytes (gfc_typenode_for_spec (element_type));
	    offset += n * element_size;
	    break;

          default:
	    gfc_error ("Bad array reference at %L", &e->where);
          }
        break;
      case REF_SUBSTRING:
        if (reference->u.ss.start != NULL)
	  offset += mpz_get_ui (*get_mpz (reference->u.ss.start)) - 1;
        break;
      default:
        gfc_error ("Illegal reference type at %L as EQUIVALENCE object",
                   &e->where);
    }
  return offset;
}


/* Add a new segment_info structure to the current segment.  eq1 is already
   in the list, eq2 is not.  */

static void
new_condition (segment_info *v, gfc_equiv *eq1, gfc_equiv *eq2)
{
  HOST_WIDE_INT offset1, offset2;
  segment_info *a;

  offset1 = calculate_offset (eq1->expr);
  offset2 = calculate_offset (eq2->expr);

  a = get_segment_info (eq2->expr->symtree->n.sym,
			v->offset + offset1 - offset2);
 
  current_segment = add_segments (current_segment, a);
}


/* Given two equivalence structures that are both already in the list, make
   sure that this new condition is not violated, generating an error if it
   is.  */

static void
confirm_condition (segment_info *s1, gfc_equiv *eq1, segment_info *s2,
                   gfc_equiv *eq2)
{
  HOST_WIDE_INT offset1, offset2;

  offset1 = calculate_offset (eq1->expr);
  offset2 = calculate_offset (eq2->expr);

  if (s1->offset + offset1 != s2->offset + offset2)
    gfc_error ("Inconsistent equivalence rules involving '%s' at %L and "
	       "'%s' at %L", s1->sym->name, &s1->sym->declared_at,
	       s2->sym->name, &s2->sym->declared_at);
}


/* Process a new equivalence condition. eq1 is know to be in segment f.
   If eq2 is also present then confirm that the condition holds.
   Otherwise add a new variable to the segment list.  */

static void
add_condition (segment_info *f, gfc_equiv *eq1, gfc_equiv *eq2)
{
  segment_info *n;

  n = find_segment_info (eq2->expr->symtree->n.sym);

  if (n == NULL)
    new_condition (f, eq1, eq2);
  else
    confirm_condition (f, eq1, n, eq2);
}


/* Given a segment element, search through the equivalence lists for unused
   conditions that involve the symbol.  Add these rules to the segment.  */

static bool
find_equivalence (segment_info *n)
{
  gfc_equiv *e1, *e2, *eq;
  bool found;

  found = FALSE;

  for (e1 = n->sym->ns->equiv; e1; e1 = e1->next)
    {
      eq = NULL;

      /* Search the equivalence list, including the root (first) element
         for the symbol that owns the segment.  */
      for (e2 = e1; e2; e2 = e2->eq)
	{
	  if (!e2->used && e2->expr->symtree->n.sym == n->sym)
	    {
	      eq = e2;
	      break;
	    }
	}

      /* Go to the next root element.  */
      if (eq == NULL)
	continue;

      eq->used = 1;

      /* Now traverse the equivalence list matching the offsets.  */
      for (e2 = e1; e2; e2 = e2->eq)
	{
	  if (!e2->used && e2 != eq)
	    {
	      add_condition (n, eq, e2);
	      e2->used = 1;
	      found = TRUE;
	    }
	}
    }
  return found;
}


/* Add all symbols equivalenced within a segment.  We need to scan the
   segment list multiple times to include indirect equivalences.  Since
   a new segment_info can inserted at the beginning of the segment list,
   depending on its offset, we have to force a final pass through the
   loop by demanding that completion sees a pass with no matches; i.e.,
   all symbols with equiv_built set and no new equivalences found.  */

static void
add_equivalences (bool *saw_equiv)
{
  segment_info *f;
  bool seen_one, more;

  seen_one = false;
  more = TRUE;
  while (more)
    {
      more = FALSE;
      for (f = current_segment; f; f = f->next)
	{
	  if (!f->sym->equiv_built)
	    {
	      f->sym->equiv_built = 1;
	      seen_one = find_equivalence (f);
	      if (seen_one)
		{
		  *saw_equiv = true;
		  more = true;
		}
	    }
	}
    }

  /* Add a copy of this segment list to the namespace.  */
  copy_equiv_list_to_ns (current_segment);
}


/* Returns the offset necessary to properly align the current equivalence.
   Sets *palign to the required alignment.  */

static HOST_WIDE_INT
align_segment (unsigned HOST_WIDE_INT *palign)
{
  segment_info *s;
  unsigned HOST_WIDE_INT offset;
  unsigned HOST_WIDE_INT max_align;
  unsigned HOST_WIDE_INT this_align;
  unsigned HOST_WIDE_INT this_offset;

  max_align = 1;
  offset = 0;
  for (s = current_segment; s; s = s->next)
    {
      this_align = TYPE_ALIGN_UNIT (s->field);
      if (s->offset & (this_align - 1))
	{
	  /* Field is misaligned.  */
	  this_offset = this_align - ((s->offset + offset) & (this_align - 1));
	  if (this_offset & (max_align - 1))
	    {
	      /* Aligning this field would misalign a previous field.  */
	      gfc_error ("The equivalence set for variable '%s' "
			 "declared at %L violates alignment requirements",
			 s->sym->name, &s->sym->declared_at);
	    }
	  offset += this_offset;
	}
      max_align = this_align;
    }
  if (palign)
    *palign = max_align;
  return offset;
}


/* Adjust segment offsets by the given amount.  */

static void
apply_segment_offset (segment_info *s, HOST_WIDE_INT offset)
{
  for (; s; s = s->next)
    s->offset += offset;
}


/* Lay out a symbol in a common block.  If the symbol has already been seen
   then check the location is consistent.  Otherwise create segments
   for that symbol and all the symbols equivalenced with it.  */

/* Translate a single common block.  */

static void
translate_common (gfc_common_head *common, gfc_symbol *var_list)
{
  gfc_symbol *sym;
  segment_info *s;
  segment_info *common_segment;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT current_offset;
  unsigned HOST_WIDE_INT align;
  bool saw_equiv;

  common_segment = NULL;
  offset = 0;
  current_offset = 0;
  align = 1;
  saw_equiv = false;

  /* Add symbols to the segment.  */
  for (sym = var_list; sym; sym = sym->common_next)
    {
      current_segment = common_segment;
      s = find_segment_info (sym);

      /* Symbol has already been added via an equivalence.  Multiple
	 use associations of the same common block result in equiv_built
	 being set but no information about the symbol in the segment.  */
      if (s && sym->equiv_built)
	{
	  /* Ensure the current location is properly aligned.  */
	  align = TYPE_ALIGN_UNIT (s->field);
	  current_offset = (current_offset + align - 1) &~ (align - 1);

	  /* Verify that it ended up where we expect it.  */
	  if (s->offset != current_offset)
	    {
	      gfc_error ("Equivalence for '%s' does not match ordering of "
			 "COMMON '%s' at %L", sym->name,
			 common->name, &common->where);
	    }
	}
      else
	{
	  /* A symbol we haven't seen before.  */
	  s = current_segment = get_segment_info (sym, current_offset);

	  /* Add all objects directly or indirectly equivalenced with this
	     symbol.  */
	  add_equivalences (&saw_equiv);

	  if (current_segment->offset < 0)
	    gfc_error ("The equivalence set for '%s' cause an invalid "
		       "extension to COMMON '%s' at %L", sym->name,
		       common->name, &common->where);

	  if (gfc_option.flag_align_commons)
	    offset = align_segment (&align);

	  if (offset)
	    {
	      /* The required offset conflicts with previous alignment
		 requirements.  Insert padding immediately before this
		 segment.  */
	      if (gfc_option.warn_align_commons)
		{
		  if (strcmp (common->name, BLANK_COMMON_NAME))
		    gfc_warning ("Padding of %d bytes required before '%s' in "
				 "COMMON '%s' at %L; reorder elements or use "
				 "-fno-align-commons", (int)offset,
				 s->sym->name, common->name, &common->where);
		  else
		    gfc_warning ("Padding of %d bytes required before '%s' in "
				 "COMMON at %L; reorder elements or use "
				 "-fno-align-commons", (int)offset,
				 s->sym->name, &common->where);
		}
	    }

	  /* Apply the offset to the new segments.  */
	  apply_segment_offset (current_segment, offset);
	  current_offset += offset;

	  /* Add the new segments to the common block.  */
	  common_segment = add_segments (common_segment, current_segment);
	}

      /* The offset of the next common variable.  */
      current_offset += s->length;
    }

  if (common_segment == NULL)
    {
      gfc_error ("COMMON '%s' at %L does not exist",
		 common->name, &common->where);
      return;
    }

  if (common_segment->offset != 0 && gfc_option.warn_align_commons)
    {
      if (strcmp (common->name, BLANK_COMMON_NAME))
	gfc_warning ("COMMON '%s' at %L requires %d bytes of padding; "
		     "reorder elements or use -fno-align-commons",
		     common->name, &common->where, (int)common_segment->offset);
      else
	gfc_warning ("COMMON at %L requires %d bytes of padding; "
		     "reorder elements or use -fno-align-commons",
		     &common->where, (int)common_segment->offset);
    }

  create_common (common, common_segment, saw_equiv);
}


/* Create a new block for each merged equivalence list.  */

static void
finish_equivalences (gfc_namespace *ns)
{
  gfc_equiv *z, *y;
  gfc_symbol *sym;
  gfc_common_head * c;
  HOST_WIDE_INT offset;
  unsigned HOST_WIDE_INT align;
  bool dummy;

  for (z = ns->equiv; z; z = z->next)
    for (y = z->eq; y; y = y->eq)
      {
        if (y->used) 
	  continue;
        sym = z->expr->symtree->n.sym;
        current_segment = get_segment_info (sym, 0);

        /* All objects directly or indirectly equivalenced with this
	   symbol.  */
        add_equivalences (&dummy);

	/* Align the block.  */
	offset = align_segment (&align);

	/* Ensure all offsets are positive.  */
	offset -= current_segment->offset & ~(align - 1);

	apply_segment_offset (current_segment, offset);

	/* Create the decl.  If this is a module equivalence, it has a
	   unique name, pointed to by z->module.  This is written to a
	   gfc_common_header to push create_common into using
	   build_common_decl, so that the equivalence appears as an
	   external symbol.  Otherwise, a local declaration is built using
	   build_equiv_decl.  */
	if (z->module)
	  {
	    c = gfc_get_common_head ();
	    /* We've lost the real location, so use the location of the
	       enclosing procedure.  */
	    c->where = ns->proc_name->declared_at;
	    strcpy (c->name, z->module);
	  }
	else
	  c = NULL;

        create_common (c, current_segment, true);
        break;
      }
}


/* Work function for translating a named common block.  */

static void
named_common (gfc_symtree *st)
{
  translate_common (st->n.common, st->n.common->head);
}


/* Translate the common blocks in a namespace. Unlike other variables,
   these have to be created before code, because the backend_decl depends
   on the rest of the common block.  */

void
gfc_trans_common (gfc_namespace *ns)
{
  gfc_common_head *c;

  /* Translate the blank common block.  */
  if (ns->blank_common.head != NULL)
    {
      c = gfc_get_common_head ();
      c->where = ns->blank_common.head->common_head->where;
      strcpy (c->name, BLANK_COMMON_NAME);
      translate_common (c, ns->blank_common.head);
    }

  /* Translate all named common blocks.  */
  gfc_traverse_symtree (ns->common_root, named_common);

  /* Translate local equivalence.  */
  finish_equivalences (ns);

  /* Commit the newly created symbols for common blocks and module
     equivalences.  */
  gfc_commit_symbols ();
}
