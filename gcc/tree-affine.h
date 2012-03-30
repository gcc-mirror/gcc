/* Operations with affine combinations of trees.
   Copyright (C) 2005, 2007, 2008 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Affine combination of trees.  We keep track of at most MAX_AFF_ELTS elements
   to make things simpler; this is sufficient in most cases.  */

#define MAX_AFF_ELTS 8

/* Element of an affine combination.  */

struct aff_comb_elt
{
  /* The value of the element.  */
  tree val;

  /* Its coefficient in the combination.  */
  double_int coef;
};

typedef struct affine_tree_combination
{
  /* Type of the result of the combination.  */
  tree type;

  /* Constant offset.  */
  double_int offset;

  /* Number of elements of the combination.  */
  unsigned n;

  /* Elements and their coefficients.  Type of elements may be different from
     TYPE, but their sizes must be the same (STRIP_NOPS is applied to the
     elements).

     The coefficients are always sign extended from the precision of TYPE
     (regardless of signedness of TYPE).  */
  struct aff_comb_elt elts[MAX_AFF_ELTS];

  /* Remainder of the expression.  Usually NULL, used only if there are more
     than MAX_AFF_ELTS elements.  Type of REST will be either sizetype for
     TYPE of POINTER_TYPEs or TYPE.  */
  tree rest;
} aff_tree;

double_int double_int_ext_for_comb (double_int, aff_tree *);
void aff_combination_const (aff_tree *, tree, double_int);
void aff_combination_elt (aff_tree *, tree, tree);
void aff_combination_scale (aff_tree *, double_int);
void aff_combination_mult (aff_tree *, aff_tree *, aff_tree *);
void aff_combination_add (aff_tree *, aff_tree *);
void aff_combination_add_elt (aff_tree *, tree, double_int);
void aff_combination_remove_elt (aff_tree *, unsigned);
void aff_combination_convert (aff_tree *, tree);
void tree_to_aff_combination (tree, tree, aff_tree *);
tree aff_combination_to_tree (aff_tree *);
void unshare_aff_combination (aff_tree *);
bool aff_combination_constant_multiple_p (aff_tree *, aff_tree *, double_int *);
void aff_combination_expand (aff_tree *, struct pointer_map_t **);
void tree_to_aff_combination_expand (tree, tree, aff_tree *,
				     struct pointer_map_t **);
void get_inner_reference_aff (tree, aff_tree *, double_int *);
void free_affine_expand_cache (struct pointer_map_t **);
bool aff_comb_cannot_overlap_p (aff_tree *, double_int, double_int);

/* Debugging functions.  */
void debug_aff (aff_tree *);
