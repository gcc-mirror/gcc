/* Data references and dependences detectors. 
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

struct data_reference GTY(())
{
  /* An identifier.  */
  unsigned int id;
  
  /* A pointer to the statement that contains this DR.  */
  tree stmt;
  
  /* A pointer to the ARRAY_REF node.  */
  tree ref;

  /* The name of the array.  */
  tree base_name;
  
  /* A list of chrecs.  */
  varray_type access_fns;

  /* Auxiliary info specific to a pass.  */
  int aux;

  /* True when the data reference is in RHS of a stmt.  */
  bool is_read;

};

#define DR_ID(DR) DR->id
#define DR_STMT(DR) DR->stmt
#define DR_REF(DR) DR->ref
#define DR_BASE_NAME(DR) DR->base_name
#define DR_ACCESS_FNS(DR) DR->access_fns
#define DR_ACCESS_FN(DR, I) VARRAY_TREE (DR_ACCESS_FNS (DR), I)
#define DR_NUM_DIMENSIONS(DR) VARRAY_ACTIVE_SIZE (DR_ACCESS_FNS (DR))
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

struct subscript GTY(()) 
{
  /* A description of the iterations for which the elements are
     accessed twice.  */
  tree conflicting_iterations_in_a;
  tree conflicting_iterations_in_b;
  
  /* These fields store the information about the iteration domain
     validity of the dependence relation.  */
  tree last_conflict_in_a;
  tree last_conflict_in_b;
  
  /* Distance from the iteration that access a conflicting element in
     A to the iteration that access this same conflicting element in
     B.  The distance is a tree scalar expression, ie. a constant or a
     symbolic expression, but certainly not a chrec function.  */
  tree distance;
  
  /* Direction (or sign) of the distance.  This more abstract (less
     precise) information is extracted from the distance field, for
     the convenience of some analyzers.  */
  enum data_dependence_direction direction;
};

#define SUB_CONFLICTS_IN_A(SUB) SUB->conflicting_iterations_in_a
#define SUB_CONFLICTS_IN_B(SUB) SUB->conflicting_iterations_in_b
#define SUB_LAST_CONFLICT_IN_A(SUB) SUB->last_conflict_in_a
#define SUB_LAST_CONFLICT_IN_B(SUB) SUB->last_conflict_in_b
#define SUB_DISTANCE(SUB) SUB->distance
#define SUB_DIRECTION(SUB) SUB->direction

/* A data_dependence_relation represents a relation between two
   data_references A and B.  */

struct data_dependence_relation GTY(())
{
  
  struct data_reference *a;
  struct data_reference *b;

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
};

#define DDR_A(DDR) DDR->a
#define DDR_B(DDR) DDR->b
#define DDR_ARE_DEPENDENT(DDR) DDR->are_dependent
#define DDR_SUBSCRIPTS(DDR) DDR->subscripts
#define DDR_SUBSCRIPTS_VECTOR_INIT(DDR, N) \
  VARRAY_GENERIC_PTR_INIT (DDR_SUBSCRIPTS (DDR), N, "subscripts_vector");
#define DDR_SUBSCRIPT(DDR, I) VARRAY_GENERIC_PTR (DDR_SUBSCRIPTS (DDR), I)
#define DDR_NUM_SUBSCRIPTS(DDR) VARRAY_ACTIVE_SIZE (DDR_SUBSCRIPTS (DDR))



struct data_dependence_relation *initialize_data_dependence_relation 
(struct data_reference *, struct data_reference *);
void compute_affine_dependence (struct data_dependence_relation *);
extern void analyze_all_data_dependences (struct loops *);
extern void compute_data_dependences_for_loop (unsigned, struct loop *, 
					       varray_type *, varray_type *, 
					       varray_type *, varray_type *);
extern struct data_reference * init_data_ref (tree, tree, tree, tree);
extern struct data_reference *analyze_array (tree, tree, bool);

extern void dump_data_reference (FILE *, struct data_reference *);
extern void dump_data_references (FILE *, varray_type);
extern void dump_data_dependence_relation (FILE *, 
					   struct data_dependence_relation *);
extern void dump_data_dependence_relations (FILE *, varray_type);
extern void dump_data_dependence_direction (FILE *, 
					    enum data_dependence_direction);



/* Inline functions.  */

/* This is the simplest data dependence test: determines whether the
   data references A and B access the same array.  */

static inline bool
array_base_name_differ_p (struct data_reference *a, 
			  struct data_reference *b)
{
  if (DR_BASE_NAME (a) == DR_BASE_NAME (b))
    return false;
  
  if (TREE_CODE (DR_BASE_NAME (a)) == INDIRECT_REF
      && TREE_CODE (DR_BASE_NAME (b)) == INDIRECT_REF
      && TREE_OPERAND (DR_BASE_NAME (a), 0) 
      == TREE_OPERAND (DR_BASE_NAME (b), 0))
    return false;
  
  return true;
}

#endif  /* GCC_TREE_DATA_REF_H  */
