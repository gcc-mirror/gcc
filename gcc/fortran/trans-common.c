/* Common block and equivalence list handling
   Copyright (C) 2000-2003 Free Software Foundation, Inc.
   Contributed by Canqun Yang <canqun@nudt.edu.cn>

This file is part of GNU G95.

G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */     

/* The core algorithm is based on Andy Vaught's g95 tree.  Also the
   way to build UNION_TYPE is borrowed from Richard Henderson.
 
   Transform common blocks.  An integral part of this is processing
   equvalence variables.  Equivalenced variables that are not in a
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
 
   Once all common blocks have been created, the list of equivalences
   is examined for still-unused equivalence conditions.  We create a
   block for each merged equivalence list.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "toplev.h"
#include "tm.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"


typedef struct segment_info
{
  gfc_symbol *sym;
  int offset;
  int length;
  tree field; 
  struct segment_info *next;
} segment_info;

static segment_info *current_segment, *current_common;
static int current_length, current_offset;
static gfc_namespace *gfc_common_ns = NULL;

#define get_segment_info() gfc_getmem (sizeof (segment_info))

#define BLANK_COMMON_NAME "__BLNK__"


/* Construct mangled common block name from symbol name.  */

static tree
gfc_sym_mangled_common_id (gfc_symbol *sym)
{
  int has_underscore;
  char name[GFC_MAX_MANGLED_SYMBOL_LEN + 1];

  if (strcmp (sym->name, BLANK_COMMON_NAME) == 0)
    return get_identifier (sym->name);
  if (gfc_option.flag_underscoring)
    {
      has_underscore = strchr (sym->name, '_') != 0;
      if (gfc_option.flag_second_underscore && has_underscore)
        snprintf (name, sizeof name, "%s__", sym->name);
      else
        snprintf (name, sizeof name, "%s_", sym->name);
      return get_identifier (name);
    }
  else
    return get_identifier (sym->name);
}


/* Build a filed declaration for a common variable or a local equivalence
   object.  */

static tree
build_field (segment_info *h, tree union_type, record_layout_info rli)
{
  tree type = gfc_sym_type (h->sym);
  tree name = get_identifier (h->sym->name);
  tree field = build_decl (FIELD_DECL, name, type);
  HOST_WIDE_INT offset = h->offset;
  unsigned int desired_align, known_align;

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
  return field;
}


/* Get storage for local equivalence.  */

static tree
build_equiv_decl (tree union_type, bool is_init)
{
  tree decl;
  decl = build_decl (VAR_DECL, NULL, union_type);
  DECL_ARTIFICIAL (decl) = 1;

  if (is_init)
    DECL_COMMON (decl) = 0;
  else
    DECL_COMMON (decl) = 1;

  TREE_ADDRESSABLE (decl) = 1;
  TREE_USED (decl) = 1;
  gfc_add_decl_to_function (decl);

  return decl;
}


/* Get storage for common block.  */

static tree
build_common_decl (gfc_symbol *sym, tree union_type, bool is_init)
{
  gfc_symbol *common_sym;
  tree decl;

  /* Create a namespace to store symbols for common blocks.  */
  if (gfc_common_ns == NULL)
    gfc_common_ns = gfc_get_namespace (NULL);

  gfc_get_symbol (sym->name, gfc_common_ns, &common_sym);
  decl = common_sym->backend_decl;

  /* Update the size of this common block as needed.  */
  if (decl != NULL_TREE)
    {
      tree size = build_int_2 (current_length, 0);
      if (tree_int_cst_lt (DECL_SIZE_UNIT (decl), size))
        {
          /* Named common blocks of the same name shall be of the same size
             in all scoping units of a program in which they appear, but
             blank common blocks may be of different sizes.  */
          if (strcmp (sym->name, BLANK_COMMON_NAME))
              gfc_warning ("named COMMON block '%s' at %L shall be of the "
                           "same size", sym->name, &sym->declared_at);
          DECL_SIZE_UNIT (decl) = size;
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
      decl = build_decl (VAR_DECL, get_identifier (sym->name), union_type);
      SET_DECL_ASSEMBLER_NAME (decl, gfc_sym_mangled_common_id (sym));
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
      DECL_ALIGN (decl) = BIGGEST_ALIGNMENT;
      DECL_USER_ALIGN (decl) = 0;
    }

  /* Has no initial values.  */
  if (!is_init)
    {
      DECL_INITIAL (decl) = NULL_TREE;
      DECL_COMMON (decl) = 1;
      DECL_DEFER_OUTPUT (decl) = 1;

      /* Place the back end declaration for this common block in
         GLOBAL_BINDING_LEVEL.  */
      common_sym->backend_decl = pushdecl_top_level (decl);
    }
  else
    {
      DECL_INITIAL (decl) = error_mark_node;
      DECL_COMMON (decl) = 0;
      DECL_DEFER_OUTPUT (decl) = 0;
      common_sym->backend_decl = decl;
    }
  return decl;
}


/* Declare memory for the common block or local equivalence, and create
   backend declarations for all of the elements.  */

static void
create_common (gfc_symbol *sym)
{ 
  segment_info *h, *next_s; 
  tree union_type;
  tree *field_link;
  record_layout_info rli;
  tree decl;
  bool is_init = false;

  /* Declare the variables inside the common block.  */
  union_type = make_node (UNION_TYPE);
  rli = start_record_layout (union_type);
  field_link = &TYPE_FIELDS (union_type);

  for (h = current_common; h; h = next_s)
    {
      tree field;
      field = build_field (h, union_type, rli);

      /* Link the field into the type.  */
      *field_link = field;
      field_link = &TREE_CHAIN (field);
      h->field = field;
      /* Has initial value.  */      
      if (h->sym->value)
        is_init = true;
    
      next_s = h->next;
    }
  finish_record_layout (rli, true);

  if (is_init)
    gfc_todo_error ("initial values for COMMON or EQUIVALENCE");
  
  if (sym)
    decl = build_common_decl (sym, union_type, is_init);
  else
    decl = build_equiv_decl (union_type, is_init);

  /* Build component reference for each variable.  */
  for (h = current_common; h; h = next_s)
    {
      h->sym->backend_decl = build (COMPONENT_REF, TREE_TYPE (h->field),
                                    decl, h->field);

      next_s = h->next;
      gfc_free (h);
    }
}   


/* Given a symbol, find it in the current segment list. Returns NULL if
   not found.  */ 

static segment_info * 
find_segment_info (gfc_symbol *symbol)
{          
  segment_info *n;

  for (n = current_segment; n; n = n->next)
    if (n->sym == symbol) return n;

  return NULL;    
} 


/* Given a variable symbol, calculate the total length in bytes of the
   variable.  */

static int
calculate_length (gfc_symbol *symbol)
{        
  int j, element_size;        
  mpz_t elements;  

  if (symbol->ts.type == BT_CHARACTER)
    gfc_conv_const_charlen (symbol->ts.cl);
  element_size = int_size_in_bytes (gfc_typenode_for_spec (&symbol->ts));
  if (symbol->as == NULL) 
    return element_size;        

  /* Calculate the number of elements in the array */  
  if (spec_size (symbol->as, &elements) == FAILURE)    
    gfc_internal_error ("calculate_length(): Unable to determine array size");
  j = mpz_get_ui (elements);          
  mpz_clear (elements);

  return j*element_size;;
}     


/* Given an expression node, make sure it is a constant integer and return
   the mpz_t value.  */     

static mpz_t * 
get_mpz (gfc_expr *g)
{
  if (g->expr_type != EXPR_CONSTANT)
    gfc_internal_error ("get_mpz(): Not an integer constant");

  return &g->value.integer;
}      


/* Given an array specification and an array reference, figure out the
   array element number (zero based). Bounds and elements are guaranteed
   to be constants.  If something goes wrong we generate an error and
   return zero.  */ 
 
static int 
element_number (gfc_array_ref *ar)
{       
  mpz_t multiplier, offset, extent, l;
  gfc_array_spec *as;
  int b, rank;

  as = ar->as;
  rank = as->rank;
  mpz_init_set_ui (multiplier, 1);
  mpz_init_set_ui (offset, 0);
  mpz_init (extent);
  mpz_init (l);

  for (b = 0; b < rank; b++)
    { 
      if (ar->dimen_type[b] != DIMEN_ELEMENT)
        gfc_internal_error ("element_number(): Bad dimension type");

      mpz_sub (l, *get_mpz (ar->start[b]), *get_mpz (as->lower[b]));
 
      mpz_mul (l, l, multiplier);
      mpz_add (offset, offset, l);
 
      mpz_sub (extent, *get_mpz (as->upper[b]), *get_mpz (as->lower[b]));
      mpz_add_ui (extent, extent, 1);
 
      if (mpz_sgn (extent) < 0)
        mpz_set_ui (extent, 0);
 
      mpz_mul (multiplier, multiplier, extent);
    } 
 
  b = mpz_get_ui (offset);
 
  mpz_clear (multiplier);
  mpz_clear (offset);
  mpz_clear (extent);
  mpz_clear (l);
 
  return b;
}


/* Given a single element of an equivalence list, figure out the offset
   from the base symbol.  For simple variables or full arrays, this is
   simply zero.  For an array element we have to calculate the array
   element number and multiply by the element size. For a substring we
   have to calculate the further reference.  */

static int
calculate_offset (gfc_expr *s)
{
  int a, element_size, offset;
  gfc_typespec *element_type;
  gfc_ref *reference;

  offset = 0;
  element_type = &s->symtree->n.sym->ts;

  for (reference = s->ref; reference; reference = reference->next)
    switch (reference->type)
      {
      case REF_ARRAY:
        switch (reference->u.ar.type)
          {
          case AR_FULL:
	    break;

          case AR_ELEMENT:
	    a = element_number (&reference->u.ar);
	    if (element_type->type == BT_CHARACTER)
	      gfc_conv_const_charlen (element_type->cl);
	    element_size =
              int_size_in_bytes (gfc_typenode_for_spec (element_type));
	    offset += a * element_size;
	    break;

          default:
	    gfc_error ("bad array reference at %L", &s->where);
          }
        break;
      case REF_SUBSTRING:
        if (reference->u.ss.start != NULL)
	  offset += mpz_get_ui (*get_mpz (reference->u.ss.start)) - 1;
        break;
      default:
        gfc_error ("illegal reference type at %L as EQUIVALENCE object",
                   &s->where);
    } 
  return offset;
}

 
/* Add a new segment_info structure to the current eq1 is already in the
   list at s1, eq2 is not.  */

static void
new_condition (segment_info *v, gfc_equiv *eq1, gfc_equiv *eq2)
{
  int offset1, offset2;
  segment_info *a;
 
  offset1 = calculate_offset (eq1->expr);
  offset2 = calculate_offset (eq2->expr);

  a = get_segment_info ();
 
  a->sym = eq2->expr->symtree->n.sym;
  a->offset = v->offset + offset1 - offset2;
  a->length = calculate_length (eq2->expr->symtree->n.sym);
 
  a->next = current_segment;
  current_segment = a;
}


/* Given two equivalence structures that are both already in the list, make
   sure that this new condition is not violated, generating an error if it
   is.  */

static void
confirm_condition (segment_info *k, gfc_equiv *eq1, segment_info *e,
                   gfc_equiv *eq2)
{
  int offset1, offset2;

  offset1 = calculate_offset (eq1->expr);
  offset2 = calculate_offset (eq2->expr);
 
  if (k->offset + offset1 != e->offset + offset2)          
    gfc_error ("inconsistent equivalence rules involving '%s' at %L and "
	       "'%s' at %L", k->sym->name, &k->sym->declared_at,
	       e->sym->name, &e->sym->declared_at);
} 

 
/* At this point we have a new equivalence condition to process. If both
   variables are already present, then we are confirming that the condition
   holds. Otherwise we are adding a new variable to the segment list.  */

static void
add_condition (gfc_equiv *eq1, gfc_equiv *eq2)
{
  segment_info *n, *t;

  eq1->expr->symtree->n.sym->mark = 1;
  eq2->expr->symtree->n.sym->mark = 1;

  eq2->used = 1;

  n = find_segment_info (eq1->expr->symtree->n.sym);
  t = find_segment_info (eq2->expr->symtree->n.sym);

  if (n == NULL && t == NULL)
    abort ();
  if (n != NULL && t == NULL)
    new_condition (n, eq1, eq2);
  if (n == NULL && t != NULL)
    new_condition (t, eq2, eq1);
  if (n != NULL && t != NULL)
    confirm_condition (n, eq1, t, eq2);
}


/* Given a symbol, search through the equivalence lists for an unused
   condition that involves the symbol.  If a rule is found, we return
   nonzero, the rule is marked as used and the eq1 and eq2 pointers point
   to the rule.  */
 
static int 
find_equivalence (gfc_symbol *sym, gfc_equiv **eq1, gfc_equiv **eq2)
{
  gfc_equiv *c, *l;
 
  for (c = sym->ns->equiv; c; c = c->next)
    for (l = c->eq; l; l = l->eq)
      {
        if (l->used) continue;

        if (c->expr->symtree->n.sym == sym || l->expr->symtree->n.sym == sym)
          {
	    *eq1 = c;
	    *eq2 = l;
	    return 1;
          }
      }
  return 0;
}

 
/* Function for adding symbols to current segment. Returns zero if the
   segment was modified.  Equivalence rules are considered to be between
   the first expression in the list and each of the other expressions in
   the list.  Symbols are scanned  multiple times because a symbol can be
   equivalenced more than once.  */

static int
add_equivalences (void)
{
  int segment_modified;
  gfc_equiv *eq1, *eq2;
  segment_info *f;

  segment_modified = 0;

  for (f = current_segment; f; f = f->next)
    if (find_equivalence (f->sym, &eq1, &eq2)) break;
 
  if (f != NULL)
    {
      add_condition (eq1, eq2);
      segment_modified = 1;
    }
 
  return segment_modified;
}
    
    
/* Given a seed symbol, create a new segment consisting of that symbol
   and all of the symbols equivalenced with that symbol.  */
 
static void
new_segment (gfc_symbol *common_sym, gfc_symbol *sym)
{
  segment_info *v;
  int length;

  current_segment = get_segment_info ();
  current_segment->sym = sym;
  current_segment->offset = current_offset;
  length = calculate_length (sym);
  current_segment->length = length;
 
  sym->mark = 1;

  /* Add all object directly or indirectly equivalenced with this common
     variable.  */ 
  while (add_equivalences ());

  /* Calculate the storage size to hold the common block.  */
  for (v = current_segment; v; v = v->next)
    {
      if (v->offset < 0)
        gfc_error ("the equivalence set for '%s' cause an invalid extension "
                   "to COMMON '%s' at %L",
                   sym->name, common_sym->name, &common_sym->declared_at);
      if (current_length < (v->offset + v->length))
        current_length = v->offset + v->length;
    }

  /* The offset of the next common variable.  */ 
  current_offset += length;

  /* Append the current segment to the current common.  */
  v = current_segment;
  while (v->next != NULL)
    v = v->next;

  v->next = current_common;
  current_common = current_segment;
  current_segment = NULL;
}


/* Create a new block for each merged equivalence list.  */

static void
finish_equivalences (gfc_namespace *ns)
{
  gfc_equiv *z, *y;
  gfc_symbol *sym;
  segment_info *v;
  int min_offset;

  for (z = ns->equiv; z; z = z->next)
    for (y= z->eq; y; y = y->eq)
      {
        if (y->used) continue;
        sym = z->expr->symtree->n.sym;
        current_length = 0;
        current_segment = get_segment_info ();
        current_segment->sym = sym;
        current_segment->offset = 0;
        current_segment->length = calculate_length (sym);
        sym->mark = 1;

        /* All object directly or indrectly equivalenced with this symbol.  */
        while (add_equivalences ());

        /* Calculate the minimal offset.  */
        min_offset = 0;
        for (v = current_segment; v; v = v->next)
          min_offset = (min_offset >= v->offset) ? v->offset : min_offset;

        /* Adjust the offset of each equivalence object, and calculate the
           maximal storage size to hold them.  */
        for (v = current_segment; v; v = v->next)
          {
            v->offset -= min_offset;
            if (current_length < (v->offset + v->length))
              current_length = v->offset + v->length;
          }

        current_common = current_segment;
        create_common (NULL);
        break;
      }
}


/* Translate a single common block.  */

static void 
translate_common (gfc_symbol *common_sym, gfc_symbol *var_list)
{
  gfc_symbol *sym;

  current_common = NULL;
  current_length = 0;
  current_offset = 0;

  /* Mark bits indicate which symbols have already been placed in a
     common area.  */
  for (sym = var_list; sym; sym = sym->common_next)
    sym->mark = 0;

  for (;;)
    {
      for (sym = var_list; sym; sym = sym->common_next)
        if (!sym->mark) break;
 
      /* All symbols have been placed in a common.  */
      if (sym == NULL) break;
      new_segment (common_sym, sym);
    }

  create_common (common_sym);
}          
 

/* Work function for translating a named common block.  */

static void
named_common (gfc_symbol *s)
{
  if (s->attr.common)
    translate_common (s, s->common_head);
}


/* Translate the common blocks in a namespace. Unlike other variables,
   these have to be created before code, because the backend_decl depends
   on the rest of the common block.  */
 
void 
gfc_trans_common (gfc_namespace *ns)
{
  gfc_symbol *sym;

  /* Translate the blank common block.  */
  if (ns->blank_common != NULL)
    {
      gfc_get_symbol (BLANK_COMMON_NAME, ns, &sym);
      translate_common (sym, ns->blank_common);
    }
 
  /* Translate all named common blocks.  */
  gfc_traverse_ns (ns, named_common); 

  /* Commit the newly created symbols for common blocks.  */
  gfc_commit_symbols ();

  /* Translate local equivalence.  */
  finish_equivalences (ns);
}
