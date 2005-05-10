/* Data references and dependences detectors. 
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_TREE_DATA_REF_H
#define GCC_TREE_DATA_REF_H

#include "lambda.h"

struct data_reference
{
  /* A pointer to the statement that contains this DR.  */
  tree stmt;
  
  /* A pointer to the ARRAY_REF node.  */
  tree ref;

  /* The name of the array.  */
  tree base_name;
  
  /* A list of chrecs.  */
  VEC(tree,heap) *access_fns;

  /* Auxiliary info specific to a pass.  */
  int aux;

  /* True when the data reference is in RHS of a stmt.  */
  bool is_read;

};

#define DR_STMT(DR) DR->stmt
#define DR_REF(DR) DR->ref
#define DR_BASE_NAME(DR) DR->base_name
#define DR_ACCESS_FNS(DR) DR->access_fns
#define DR_ACCESS_FN(DR, I) VEC_index (tree, DR_ACCESS_FNS (DR), I)
#define DR_NUM_DIMENSIONS(DR) VEC_length (tree, DR_ACCESS_FNS (DR))
#define DR_IS_READ(DR) DR->is_read

enum data_dependence_direction {
  dir_positive, 
  dir_negative, 
  dir_equal, 
  dir_positive_or_negative,
  dir_positive_or_equal,
  dir_negative_or_equal,
  dir_star,
  dir_independent
};

/* What is a subscript?  Given two array accesses a subscript is the
   tuple composed of the access functions for a given dimension.
   Example: Given A[f1][f2][f3] and B[g1][g2][g3], there are three
   subscripts: (f1, g1), (f2, g2), (f3, g3).  These three subscripts
   are stored in the data_dependence_relation structure under the form
   of an array of subscripts.  */

struct subscript
{
  /* A description of the iterations for which the elements are
     accessed twice.  */
  tree conflicting_iterations_in_a;
  tree conflicting_iterations_in_b;
  
  /* This field stores the information about the iteration domain
     validity of the dependence relation.  */
  tree last_conflict;
  
  /* Distance from the iteration that access a conflicting element in
     A to the iteration that access this same conflicting element in
     B.  The distance is a tree scalar expression, i.e. a constant or a
     symbolic expression, but certainly not a chrec function.  */
  tree distance;
};

#define SUB_CONFLICTS_IN_A(SUB) SUB->conflicting_iterations_in_a
#define SUB_CONFLICTS_IN_B(SUB) SUB->conflicting_iterations_in_b
#define SUB_LAST_CONFLICT(SUB) SUB->last_conflict
#define SUB_DISTANCE(SUB) SUB->distance

/* A data_dependence_relation represents a relation between two
   data_references A and B.  */

struct data_dependence_relation
{
  
  struct data_reference *a;
  struct data_reference *b;

  /* When the dependence relation is affine, it can be represented by
     a distance vector.  */
  bool affine_p;

  /* A "yes/no/maybe" field for the dependence relation:
     
     - when "ARE_DEPENDENT == NULL_TREE", there exist a dependence
       relation between A and B, and the description of this relation
       is given in the SUBSCRIPTS array,
     
     - when "ARE_DEPENDENT == chrec_known", there is no dependence and
       SUBSCRIPTS is empty,
     
     - when "ARE_DEPENDENT == chrec_dont_know", there may be a dependence,
       but the analyzer cannot be more specific.  */
  tree are_dependent;
  
  /* For each subscript in the dependence test, there is an element in
     this array.  This is the attribute that labels the edge A->B of
     the data_dependence_relation.  */
  varray_type subscripts;

  /* The size of the direction/distance vectors.  */
  int size_vect;

  /* The classic direction vector.  */
  lambda_vector dir_vect;

  /* The classic distance vector.  */
  lambda_vector dist_vect;
};

#define DDR_A(DDR) DDR->a
#define DDR_B(DDR) DDR->b
#define DDR_AFFINE_P(DDR) DDR->affine_p
#define DDR_ARE_DEPENDENT(DDR) DDR->are_dependent
#define DDR_SUBSCRIPTS(DDR) DDR->subscripts
#define DDR_SUBSCRIPTS_VECTOR_INIT(DDR, N) \
  VARRAY_GENERIC_PTR_INIT (DDR_SUBSCRIPTS (DDR), N, "subscripts_vector");
#define DDR_SUBSCRIPT(DDR, I) VARRAY_GENERIC_PTR (DDR_SUBSCRIPTS (DDR), I)
#define DDR_NUM_SUBSCRIPTS(DDR) VARRAY_ACTIVE_SIZE (DDR_SUBSCRIPTS (DDR))
#define DDR_SIZE_VECT(DDR) DDR->size_vect
#define DDR_DIR_VECT(DDR) DDR->dir_vect
#define DDR_DIST_VECT(DDR) DDR->dist_vect



extern tree find_data_references_in_loop (struct loop *, varray_type *);
extern struct data_dependence_relation *initialize_data_dependence_relation 
(struct data_reference *, struct data_reference *);
extern void compute_affine_dependence (struct data_dependence_relation *);
extern void analyze_all_data_dependences (struct loops *);
extern void compute_data_dependences_for_loop (unsigned, struct loop *, 
					       varray_type *, varray_type *);
extern struct data_reference * init_data_ref (tree, tree, tree, tree, bool);
extern struct data_reference *analyze_array (tree, tree, bool);

extern void dump_subscript (FILE *, struct subscript *);
extern void dump_ddrs (FILE *, varray_type);
extern void dump_dist_dir_vectors (FILE *, varray_type);
extern void dump_data_reference (FILE *, struct data_reference *);
extern void dump_data_references (FILE *, varray_type);
extern void dump_data_dependence_relation (FILE *, 
					   struct data_dependence_relation *);
extern void dump_data_dependence_relations (FILE *, varray_type);
extern void dump_data_dependence_direction (FILE *, 
					    enum data_dependence_direction);
extern bool array_base_name_differ_p (struct data_reference *, 
				      struct data_reference *, bool *);
extern void free_dependence_relation (struct data_dependence_relation *);
extern void free_dependence_relations (varray_type);
extern void free_data_refs (varray_type);
extern void compute_subscript_distance (struct data_dependence_relation *);
extern bool build_classic_dist_vector (struct data_dependence_relation *, int, int);




#endif  /* GCC_TREE_DATA_REF_H  */
