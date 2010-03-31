/* Graphite polyhedral representation.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Tobias Grosser <grosser@fim.uni-passau.de>.

This file is part of GCC.

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

#ifndef GCC_GRAPHITE_POLY_H
#define GCC_GRAPHITE_POLY_H

typedef struct poly_dr *poly_dr_p;
DEF_VEC_P(poly_dr_p);
DEF_VEC_ALLOC_P (poly_dr_p, heap);

typedef struct poly_bb *poly_bb_p;
DEF_VEC_P(poly_bb_p);
DEF_VEC_ALLOC_P (poly_bb_p, heap);

typedef struct scop *scop_p;
DEF_VEC_P(scop_p);
DEF_VEC_ALLOC_P (scop_p, heap);

typedef ppl_dimension_type graphite_dim_t;

static inline graphite_dim_t pbb_dim_iter_domain (const struct poly_bb *);
static inline graphite_dim_t pbb_nb_params (const struct poly_bb *);
static inline graphite_dim_t scop_nb_params (scop_p);

/* A data reference can write or read some memory or we
   just know it may write some memory.  */
enum poly_dr_type
{
  PDR_READ,
  /* PDR_MAY_READs are represented using PDR_READS.  This does not
     limit the expressiveness.  */
  PDR_WRITE,
  PDR_MAY_WRITE
};

struct poly_dr
{
  /* An identifier for this PDR.  */
  int id;

  /* The number of data refs identical to this one in the PBB.  */
  int nb_refs;

  /* A pointer to compiler's data reference description.  */
  void *compiler_dr;

  /* A pointer to the PBB that contains this data reference.  */
  poly_bb_p pbb;

  enum poly_dr_type type;

  /* The access polyhedron contains the polyhedral space this data
     reference will access.

     The polyhedron contains these dimensions:

     - The alias set (a):
     Every memory access is classified in at least one alias set.

     - The subscripts (s_0, ..., s_n):
     The memory is accessed using zero or more subscript dimensions.

     - The iteration domain (variables and parameters)

     Do not hardcode the dimensions.  Use the following accessor functions:
     - pdr_alias_set_dim
     - pdr_subscript_dim
     - pdr_iterator_dim
     - pdr_parameter_dim

     Example:

     | int A[1335][123];
     | int *p = malloc ();
     |
     | k = ...
     | for i
     |   {
     |     if (unknown_function ())
     |       p = A;
     |       ... = p[?][?];
     | 	   for j
     |       A[i][j+k] = m;
     |   }

     The data access A[i][j+k] in alias set "5" is described like this:

     | i   j   k   a  s0  s1   1
     | 0   0   0   1   0   0  -5     =  0
     |-1   0   0   0   1   0   0     =  0
     | 0  -1  -1   0   0   1   0     =  0
     | 0   0   0   0   1   0   0     >= 0  # The last four lines describe the
     | 0   0   0   0   0   1   0     >= 0  # array size.
     | 0   0   0   0  -1   0 1335    >= 0
     | 0   0   0   0   0  -1 123     >= 0

     The pointer "*p" in alias set "5" and "7" is described as a union of
     polyhedron:


     | i   k   a  s0   1
     | 0   0   1   0  -5   =  0
     | 0   0   0   1   0   >= 0

     "or"

     | i   k   a  s0   1
     | 0   0   1   0  -7   =  0
     | 0   0   0   1   0   >= 0

     "*p" accesses all of the object allocated with 'malloc'.

     The scalar data access "m" is represented as an array with zero subscript
     dimensions.

     | i   j   k   a   1
     | 0   0   0  -1   15  = 0 */
  ppl_Pointset_Powerset_C_Polyhedron_t accesses;

  /* Data reference's base object set number, we must assure 2 pdrs are in the
     same base object set before dependency checking.  */
  int dr_base_object_set;

  /* The number of subscripts.  */
  graphite_dim_t nb_subscripts;
};

#define PDR_ID(PDR) (PDR->id)
#define PDR_NB_REFS(PDR) (PDR->nb_refs)
#define PDR_CDR(PDR) (PDR->compiler_dr)
#define PDR_PBB(PDR) (PDR->pbb)
#define PDR_TYPE(PDR) (PDR->type)
#define PDR_ACCESSES(PDR) (PDR->accesses)
#define PDR_BASE_OBJECT_SET(PDR) (PDR->dr_base_object_set)
#define PDR_NB_SUBSCRIPTS(PDR) (PDR->nb_subscripts)

void new_poly_dr (poly_bb_p, int, ppl_Pointset_Powerset_C_Polyhedron_t,
		  enum poly_dr_type, void *, graphite_dim_t);
void free_poly_dr (poly_dr_p);
void debug_pdr (poly_dr_p);
void print_pdr (FILE *, poly_dr_p);
static inline scop_p pdr_scop (poly_dr_p pdr);

/* The dimension of the PDR_ACCESSES polyhedron of PDR.  */

static inline ppl_dimension_type
pdr_dim (poly_dr_p pdr)
{
  ppl_dimension_type dim;
  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (PDR_ACCESSES (pdr),
						      &dim);
  return dim;
}

/* The dimension of the iteration domain of the scop of PDR.  */

static inline ppl_dimension_type
pdr_dim_iter_domain (poly_dr_p pdr)
{
  return pbb_dim_iter_domain (PDR_PBB (pdr));
}

/* The number of parameters of the scop of PDR.  */

static inline ppl_dimension_type
pdr_nb_params (poly_dr_p pdr)
{
  return scop_nb_params (pdr_scop (pdr));
}

/* The dimension of the alias set in PDR.  */

static inline ppl_dimension_type
pdr_alias_set_dim (poly_dr_p pdr)
{
  poly_bb_p pbb = PDR_PBB (pdr);

  return pbb_dim_iter_domain (pbb) + pbb_nb_params (pbb);
}

/* The dimension in PDR containing subscript S.  */

static inline ppl_dimension_type
pdr_subscript_dim (poly_dr_p pdr, graphite_dim_t s)
{
  poly_bb_p pbb = PDR_PBB (pdr);

  return pbb_dim_iter_domain (pbb) + pbb_nb_params (pbb) + 1 + s;
}

/* The dimension in PDR containing the loop iterator ITER.  */

static inline ppl_dimension_type
pdr_iterator_dim (poly_dr_p pdr ATTRIBUTE_UNUSED, graphite_dim_t iter)
{
  return iter;
}

/* The dimension in PDR containing parameter PARAM.  */

static inline ppl_dimension_type
pdr_parameter_dim (poly_dr_p pdr, graphite_dim_t param)
{
  poly_bb_p pbb = PDR_PBB (pdr);

  return pbb_dim_iter_domain (pbb) + param;
}

/* Returns true when PDR is a "read".  */

static inline bool
pdr_read_p (poly_dr_p pdr)
{
  return PDR_TYPE (pdr) == PDR_READ;
}

/* Returns true when PDR is a "write".  */

static inline bool
pdr_write_p (poly_dr_p pdr)
{
  return PDR_TYPE (pdr) == PDR_WRITE;
}

/* Returns true when PDR is a "may write".  */

static inline bool
pdr_may_write_p (poly_dr_p pdr)
{
  return PDR_TYPE (pdr) == PDR_MAY_WRITE;
}

/* Return true when PDR1 and PDR2 are similar data accesses: they have
   the same base array, and the same access functions.  */

static inline bool
same_pdr_p (poly_dr_p pdr1, poly_dr_p pdr2)
{
  return PDR_TYPE (pdr1) == PDR_TYPE (pdr2)
    && PDR_NB_SUBSCRIPTS (pdr1) == PDR_NB_SUBSCRIPTS (pdr2)
    && PDR_BASE_OBJECT_SET (pdr1) == PDR_BASE_OBJECT_SET (pdr2);
}

typedef struct poly_scattering *poly_scattering_p;

struct poly_scattering
{
  /* The scattering function containing the transformations: the
     layout of this polyhedron is: T|I|G with T the transform
     scattering, I the iteration domain, G the context parameters.  */
  ppl_Polyhedron_t scattering;

  /* The number of local variables.  */
  int nb_local_variables;

  /* The number of scattering dimensions.  */
  int nb_scattering;
};

/* POLY_BB represents a blackbox in the polyhedral model.  */

struct poly_bb
{
  /* Pointer to a basic block or a statement in the compiler.  */
  void *black_box;

  /* Pointer to the SCOP containing this PBB.  */
  scop_p scop;

  /* The iteration domain of this bb.  The layout of this polyhedron
     is I|G with I the iteration domain, G the context parameters.

     Example:

     for (i = a - 7*b + 8; i <= 3*a + 13*b + 20; i++)
       for (j = 2; j <= 2*i + 5; j++)
         for (k = 0; k <= 5; k++)
           S (i,j,k)

     Loop iterators: i, j, k
     Parameters: a, b

     | i >=  a -  7b +  8
     | i <= 3a + 13b + 20
     | j >= 2
     | j <= 2i + 5
     | k >= 0
     | k <= 5

     The number of variables in the DOMAIN may change and is not
     related to the number of loops in the original code.  */
  ppl_Pointset_Powerset_C_Polyhedron_t domain;

  /* The data references we access.  */
  VEC (poly_dr_p, heap) *drs;

  /* The original scattering.  */
  poly_scattering_p original;

  /* The transformed scattering.  */
  poly_scattering_p transformed;

  /* A copy of the transformed scattering.  */
  poly_scattering_p saved;

  /* True when the PDR duplicates have already been removed.  */
  bool pdr_duplicates_removed;

  /* True when this PBB contains only a reduction statement.  */
  bool is_reduction;
};

#define PBB_BLACK_BOX(PBB) ((gimple_bb_p) PBB->black_box)
#define PBB_SCOP(PBB) (PBB->scop)
#define PBB_DOMAIN(PBB) (PBB->domain)
#define PBB_DRS(PBB) (PBB->drs)
#define PBB_ORIGINAL(PBB) (PBB->original)
#define PBB_ORIGINAL_SCATTERING(PBB) (PBB->original->scattering)
#define PBB_TRANSFORMED(PBB) (PBB->transformed)
#define PBB_TRANSFORMED_SCATTERING(PBB) (PBB->transformed->scattering)
#define PBB_SAVED(PBB) (PBB->saved)
#define PBB_NB_LOCAL_VARIABLES(PBB) (PBB->transformed->nb_local_variables)
#define PBB_NB_SCATTERING_TRANSFORM(PBB) (PBB->transformed->nb_scattering)
#define PBB_PDR_DUPLICATES_REMOVED(PBB) (PBB->pdr_duplicates_removed)
#define PBB_IS_REDUCTION(PBB) (PBB->is_reduction)

extern void new_poly_bb (scop_p, void *, bool);
extern void free_poly_bb (poly_bb_p);
extern void debug_loop_vec (poly_bb_p);
extern void schedule_to_scattering (poly_bb_p, int);
extern void print_pbb_domain (FILE *, poly_bb_p);
extern void print_pbb (FILE *, poly_bb_p);
extern void print_scop_context (FILE *, scop_p);
extern void print_scop (FILE *, scop_p);
extern void print_cloog (FILE *, scop_p);
extern void debug_pbb_domain (poly_bb_p);
extern void debug_pbb (poly_bb_p);
extern void print_pdrs (FILE *, poly_bb_p);
extern void debug_pdrs (poly_bb_p);
extern void debug_scop_context (scop_p);
extern void debug_scop (scop_p);
extern void debug_cloog (scop_p);
extern void print_scop_params (FILE *, scop_p);
extern void debug_scop_params (scop_p);
extern void print_iteration_domain (FILE *, poly_bb_p);
extern void print_iteration_domains (FILE *, scop_p);
extern void debug_iteration_domain (poly_bb_p);
extern void debug_iteration_domains (scop_p);
extern bool scop_do_interchange (scop_p);
extern bool scop_do_strip_mine (scop_p);
extern bool scop_do_block (scop_p);
extern void pbb_number_of_iterations (poly_bb_p, graphite_dim_t, Value);
extern void pbb_number_of_iterations_at_time (poly_bb_p, graphite_dim_t, Value);
extern void pbb_remove_duplicate_pdrs (poly_bb_p);

/* Return the number of write data references in PBB.  */

static inline int
number_of_write_pdrs (poly_bb_p pbb)
{
  int res = 0;
  int i;
  poly_dr_p pdr;

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
    if (PDR_TYPE (pdr) == PDR_WRITE)
      res++;

  return res;
}

/* The basic block of the PBB.  */
static inline basic_block
pbb_bb (poly_bb_p pbb)
{
  return GBB_BB (PBB_BLACK_BOX (pbb));
}

/* The index of the PBB.  */

static inline int
pbb_index (poly_bb_p pbb)
{
  return pbb_bb (pbb)->index;
}

/* The loop of the PBB.  */

static inline loop_p
pbb_loop (poly_bb_p pbb)
{
  return gbb_loop (PBB_BLACK_BOX (pbb));
}

/* The scop that contains the PDR.  */

static inline scop_p
pdr_scop (poly_dr_p pdr)
{
  return PBB_SCOP (PDR_PBB (pdr));
}

/* Set black box of PBB to BLACKBOX.  */

static inline void
pbb_set_black_box (poly_bb_p pbb, void *black_box)
{
  pbb->black_box = black_box;
}

/* The number of loops around PBB: the dimension of the iteration
   domain.  */

static inline graphite_dim_t
pbb_dim_iter_domain (const struct poly_bb *pbb)
{
  scop_p scop = PBB_SCOP (pbb);
  ppl_dimension_type dim;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (PBB_DOMAIN (pbb), &dim);
  return dim - scop_nb_params (scop);
}

/* The number of params defined in PBB.  */

static inline graphite_dim_t
pbb_nb_params (const struct poly_bb *pbb)
{
  scop_p scop = PBB_SCOP (pbb);

  return scop_nb_params (scop);
}

/* The number of scattering dimensions in the SCATTERING polyhedron
   of a PBB for a given SCOP.  */

static inline graphite_dim_t
pbb_nb_scattering_orig (const struct poly_bb *pbb)
{
  return 2 * pbb_dim_iter_domain (pbb) + 1;
}

/* The number of scattering dimensions in PBB.  */

static inline graphite_dim_t
pbb_nb_scattering_transform (const struct poly_bb *pbb)
{
  return PBB_NB_SCATTERING_TRANSFORM (pbb);
}

/* The number of dynamic scattering dimensions in PBB.  */

static inline graphite_dim_t
pbb_nb_dynamic_scattering_transform (const struct poly_bb *pbb)
{
  /* This function requires the 2d + 1 scattering format to be
     invariant during all transformations.  */
  gcc_assert (PBB_NB_SCATTERING_TRANSFORM (pbb) % 2);
  return PBB_NB_SCATTERING_TRANSFORM (pbb) / 2;
}

/* Returns the number of local variables used in the transformed
   scattering polyhedron of PBB.  */

static inline graphite_dim_t
pbb_nb_local_vars (const struct poly_bb *pbb)
{
  /* For now we do not have any local variables, as we do not do strip
     mining for example.  */
  return PBB_NB_LOCAL_VARIABLES (pbb);
}

/* The dimension in the domain of PBB containing the iterator ITER.  */

static inline ppl_dimension_type
pbb_iterator_dim (poly_bb_p pbb ATTRIBUTE_UNUSED, graphite_dim_t iter)
{
  return iter;
}

/* The dimension in the domain of PBB containing the iterator ITER.  */

static inline ppl_dimension_type
pbb_parameter_dim (poly_bb_p pbb, graphite_dim_t param)
{
  return param
    + pbb_dim_iter_domain (pbb);
}

/* The dimension in the original scattering polyhedron of PBB
   containing the scattering iterator SCATTER.  */

static inline ppl_dimension_type
psco_scattering_dim (poly_bb_p pbb ATTRIBUTE_UNUSED, graphite_dim_t scatter)
{
  gcc_assert (scatter < pbb_nb_scattering_orig (pbb));
  return scatter;
}

/* The dimension in the transformed scattering polyhedron of PBB
   containing the scattering iterator SCATTER.  */

static inline ppl_dimension_type
psct_scattering_dim (poly_bb_p pbb ATTRIBUTE_UNUSED, graphite_dim_t scatter)
{
  gcc_assert (scatter <= pbb_nb_scattering_transform (pbb));
  return scatter;
}

ppl_dimension_type psct_scattering_dim_for_loop_depth (poly_bb_p,
						       graphite_dim_t);

/* The dimension in the transformed scattering polyhedron of PBB of
   the local variable LV.  */

static inline ppl_dimension_type
psct_local_var_dim (poly_bb_p pbb, graphite_dim_t lv)
{
  gcc_assert (lv <= pbb_nb_local_vars (pbb));
  return lv + pbb_nb_scattering_transform (pbb);
}

/* The dimension in the original scattering polyhedron of PBB
   containing the loop iterator ITER.  */

static inline ppl_dimension_type
psco_iterator_dim (poly_bb_p pbb, graphite_dim_t iter)
{
  gcc_assert (iter < pbb_dim_iter_domain (pbb));
  return iter + pbb_nb_scattering_orig (pbb);
}

/* The dimension in the transformed scattering polyhedron of PBB
   containing the loop iterator ITER.  */

static inline ppl_dimension_type
psct_iterator_dim (poly_bb_p pbb, graphite_dim_t iter)
{
  gcc_assert (iter < pbb_dim_iter_domain (pbb));
  return iter
    + pbb_nb_scattering_transform (pbb)
    + pbb_nb_local_vars (pbb);
}

/* The dimension in the original scattering polyhedron of PBB
   containing parameter PARAM.  */

static inline ppl_dimension_type
psco_parameter_dim (poly_bb_p pbb, graphite_dim_t param)
{
  gcc_assert (param < pbb_nb_params (pbb));
  return param
    + pbb_nb_scattering_orig (pbb)
    + pbb_dim_iter_domain (pbb);
}

/* The dimension in the transformed scattering polyhedron of PBB
   containing parameter PARAM.  */

static inline ppl_dimension_type
psct_parameter_dim (poly_bb_p pbb, graphite_dim_t param)
{
  gcc_assert (param < pbb_nb_params (pbb));
  return param
    + pbb_nb_scattering_transform (pbb)
    + pbb_nb_local_vars (pbb)
    + pbb_dim_iter_domain (pbb);
}

/* The scattering dimension of PBB corresponding to the dynamic level
   LEVEL.  */

static inline ppl_dimension_type
psct_dynamic_dim (poly_bb_p pbb, graphite_dim_t level)
{
  graphite_dim_t result = 1 + 2 * level;

  gcc_assert (result < pbb_nb_scattering_transform (pbb));
  return result;
}

/* The scattering dimension of PBB corresponding to the static
   sequence of the loop level LEVEL.  */

static inline ppl_dimension_type
psct_static_dim (poly_bb_p pbb, graphite_dim_t level)
{
  graphite_dim_t result = 2 * level;

  gcc_assert (result < pbb_nb_scattering_transform (pbb));
  return result;
}

/* Adds to the transformed scattering polyhedron of PBB a new local
   variable and returns its index.  */

static inline graphite_dim_t
psct_add_local_variable (poly_bb_p pbb)
{
  graphite_dim_t nlv = pbb_nb_local_vars (pbb);
  ppl_dimension_type lv_column = psct_local_var_dim (pbb, nlv);
  ppl_insert_dimensions (PBB_TRANSFORMED_SCATTERING (pbb), lv_column, 1);
  PBB_NB_LOCAL_VARIABLES (pbb) += 1;
  return nlv;
}

/* Adds a dimension to the transformed scattering polyhedron of PBB at
   INDEX.  */

static inline void
psct_add_scattering_dimension (poly_bb_p pbb, ppl_dimension_type index)
{
  gcc_assert (index < pbb_nb_scattering_transform (pbb));

  ppl_insert_dimensions (PBB_TRANSFORMED_SCATTERING (pbb), index, 1);
  PBB_NB_SCATTERING_TRANSFORM (pbb) += 1;
}

typedef struct lst *lst_p;
DEF_VEC_P(lst_p);
DEF_VEC_ALLOC_P (lst_p, heap);

/* Loops and Statements Tree.  */
struct lst {

  /* LOOP_P is true when an LST node is a loop.  */
  bool loop_p;

  /* A pointer to the loop that contains this node.  */
  lst_p loop_father;

  /* The sum of all the memory strides for an LST loop.  */
  Value memory_strides;

  /* Loop nodes contain a sequence SEQ of LST nodes, statements
     contain a pointer to their polyhedral representation PBB.  */
  union {
    poly_bb_p pbb;
    VEC (lst_p, heap) *seq;
  } node;
};

#define LST_LOOP_P(LST) ((LST)->loop_p)
#define LST_LOOP_FATHER(LST) ((LST)->loop_father)
#define LST_PBB(LST) ((LST)->node.pbb)
#define LST_SEQ(LST) ((LST)->node.seq)
#define LST_LOOP_MEMORY_STRIDES(LST) ((LST)->memory_strides)

void scop_to_lst (scop_p);
void print_lst (FILE *, lst_p, int);
void debug_lst (lst_p);
void dot_lst (lst_p);

/* Creates a new LST loop with SEQ.  */

static inline lst_p
new_lst_loop (VEC (lst_p, heap) *seq)
{
  lst_p lst = XNEW (struct lst);
  int i;
  lst_p l;

  LST_LOOP_P (lst) = true;
  LST_SEQ (lst) = seq;
  LST_LOOP_FATHER (lst) = NULL;
  value_init (LST_LOOP_MEMORY_STRIDES (lst));
  value_set_si (LST_LOOP_MEMORY_STRIDES (lst), -1);

  for (i = 0; VEC_iterate (lst_p, seq, i, l); i++)
    LST_LOOP_FATHER (l) = lst;

  return lst;
}

/* Creates a new LST statement with PBB.  */

static inline lst_p
new_lst_stmt (poly_bb_p pbb)
{
  lst_p lst = XNEW (struct lst);

  LST_LOOP_P (lst) = false;
  LST_PBB (lst) = pbb;
  LST_LOOP_FATHER (lst) = NULL;
  return lst;
}

/* Frees the memory used by LST.  */

static inline void
free_lst (lst_p lst)
{
  if (!lst)
    return;

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;

      for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
	free_lst (l);

      value_clear (LST_LOOP_MEMORY_STRIDES (lst));
      VEC_free (lst_p, heap, LST_SEQ (lst));
    }

  free (lst);
}

/* Returns a copy of LST.  */

static inline lst_p
copy_lst (lst_p lst)
{
  if (!lst)
    return NULL;

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;
      VEC (lst_p, heap) *seq = VEC_alloc (lst_p, heap, 5);

      for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
	VEC_safe_push (lst_p, heap, seq, copy_lst (l));

      return new_lst_loop (seq);
    }

  return new_lst_stmt (LST_PBB (lst));
}

/* Adds a new loop under the loop LST.  */

static inline void
lst_add_loop_under_loop (lst_p lst)
{
  VEC (lst_p, heap) *seq = VEC_alloc (lst_p, heap, 1);
  lst_p l = new_lst_loop (LST_SEQ (lst));

  gcc_assert (LST_LOOP_P (lst));

  LST_LOOP_FATHER (l) = lst;
  VEC_quick_push (lst_p, seq, l);
  LST_SEQ (lst) = seq;
}

/* Returns the loop depth of LST.  */

static inline int
lst_depth (lst_p lst)
{
  if (!lst)
    return -2;

  /* The depth of the outermost "fake" loop is -1.  This outermost
     loop does not have a loop father and it is just a container, as
     in the loop representation of GCC.  */
  if (!LST_LOOP_FATHER (lst))
    return -1;

  return lst_depth (LST_LOOP_FATHER (lst)) + 1;
}

/* Returns the Dewey number for LST.  */

static inline int
lst_dewey_number (lst_p lst)
{
  int i;
  lst_p l;

  if (!lst)
    return -1;

  if (!LST_LOOP_FATHER (lst))
    return 0;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (LST_LOOP_FATHER (lst)), i, l); i++)
    if (l == lst)
      return i;

  return -1;
}

/* Returns the Dewey number of LST at depth DEPTH.  */

static inline int
lst_dewey_number_at_depth (lst_p lst, int depth)
{
  gcc_assert (lst && depth >= 0 && lst_depth (lst) <= depth);

  if (lst_depth (lst) == depth)
    return lst_dewey_number (lst);

  return lst_dewey_number_at_depth (LST_LOOP_FATHER (lst), depth);
}

/* Returns the predecessor of LST in the sequence of its loop father.
   Returns NULL if LST is the first statement in the sequence.  */

static inline lst_p
lst_pred (lst_p lst)
{
  int dewey;
  lst_p father;

  if (!lst || !LST_LOOP_FATHER (lst))
    return NULL;

  dewey = lst_dewey_number (lst);
  if (dewey == 0)
    return NULL;

  father = LST_LOOP_FATHER (lst);
  return VEC_index (lst_p, LST_SEQ (father), dewey - 1);
}

/* Returns the successor of LST in the sequence of its loop father.
   Returns NULL if there is none.  */

static inline lst_p
lst_succ (lst_p lst)
{
  int dewey;
  lst_p father;

  if (!lst || !LST_LOOP_FATHER (lst))
    return NULL;

  dewey = lst_dewey_number (lst);
  father = LST_LOOP_FATHER (lst);

  if (VEC_length (lst_p, LST_SEQ (father)) == (unsigned) dewey + 1)
    return NULL;

  return VEC_index (lst_p, LST_SEQ (father), dewey + 1);
}


/* Return the LST node corresponding to PBB.  */

static inline lst_p
lst_find_pbb (lst_p lst, poly_bb_p pbb)
{
  int i;
  lst_p l;

  if (!lst)
    return NULL;

  if (!LST_LOOP_P (lst))
    return (pbb == LST_PBB (lst)) ? lst : NULL;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
    {
      lst_p res = lst_find_pbb (l, pbb);
      if (res)
	return res;
    }

  return NULL;
}

/* Return the LST node corresponding to the loop around STMT at depth
   LOOP_DEPTH.  */

static inline lst_p
find_lst_loop (lst_p stmt, int loop_depth)
{
  lst_p loop = LST_LOOP_FATHER (stmt);

  gcc_assert (loop_depth >= 0);

  while (loop_depth < lst_depth (loop))
    loop = LST_LOOP_FATHER (loop);

  return loop;
}

/* Return the first lst representing a PBB statement in LST.  */

static inline lst_p
lst_find_first_pbb (lst_p lst)
{
  int i;
  lst_p l;

  if (!lst)
    return NULL;

  if (!LST_LOOP_P (lst))
    return lst;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
    {
      lst_p res = lst_find_first_pbb (l);
      if (res)
	return res;
    }

  return NULL;
}

/* Returns true when LST is a loop that does not contains
   statements.  */

static inline bool
lst_empty_p (lst_p lst)
{
  return !lst_find_first_pbb (lst);
}

/* Return the last lst representing a PBB statement in LST.  */

static inline lst_p
lst_find_last_pbb (lst_p lst)
{
  int i;
  lst_p l, res = NULL;

  if (!lst)
    return NULL;

  if (!LST_LOOP_P (lst))
    return lst;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
    {
      lst_p last = lst_find_last_pbb (l);

      if (last)
	res = last;
    }

  gcc_assert (res);
  return res;
}

/* Returns true if LOOP contains LST, in other words, if LST is nested
   in LOOP.  */

static inline bool
lst_contains_p (lst_p loop, lst_p lst)
{
  if (!loop || !lst || !LST_LOOP_P (loop))
    return false;

  if (loop == lst)
    return true;

  return lst_contains_p (loop, LST_LOOP_FATHER (lst));
}

/* Returns true if LOOP contains PBB, in other words, if PBB is nested
   in LOOP.  */

static inline bool
lst_contains_pbb (lst_p loop, poly_bb_p pbb)
{
  return lst_find_pbb (loop, pbb) ? true : false;
}

/* Creates a loop nest of depth NB_LOOPS containing LST.  */

static inline lst_p
lst_create_nest (int nb_loops, lst_p lst)
{
  lst_p res, loop;
  VEC (lst_p, heap) *seq;

  if (nb_loops == 0)
    return lst;

  seq = VEC_alloc (lst_p, heap, 1);
  loop = lst_create_nest (nb_loops - 1, lst);
  VEC_quick_push (lst_p, seq, loop);
  res = new_lst_loop (seq);
  LST_LOOP_FATHER (loop) = res;

  return res;
}

/* Removes LST from the sequence of statements of its loop father.  */

static inline void
lst_remove_from_sequence (lst_p lst)
{
  lst_p father = LST_LOOP_FATHER (lst);
  int dewey = lst_dewey_number (lst);

  gcc_assert (lst && father && dewey >= 0);

  VEC_ordered_remove (lst_p, LST_SEQ (father), dewey);
  LST_LOOP_FATHER (lst) = NULL;
}

/* Updates the scattering of PBB to be at the DEWEY number in the loop
   at depth LEVEL.  */

static inline void
pbb_update_scattering (poly_bb_p pbb, graphite_dim_t level, int dewey)
{
  ppl_Polyhedron_t ph = PBB_TRANSFORMED_SCATTERING (pbb);
  ppl_dimension_type sched = psct_static_dim (pbb, level);
  ppl_dimension_type ds[1];
  ppl_Constraint_t new_cstr;
  ppl_Linear_Expression_t expr;
  ppl_dimension_type dim;

  ppl_Polyhedron_space_dimension (ph, &dim);
  ds[0] = sched;
  ppl_Polyhedron_remove_space_dimensions (ph, ds, 1);
  ppl_insert_dimensions (ph, sched, 1);

  ppl_new_Linear_Expression_with_dimension (&expr, dim);
  ppl_set_coef (expr, sched, -1);
  ppl_set_inhomogeneous (expr, dewey);
  ppl_new_Constraint (&new_cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
  ppl_delete_Linear_Expression (expr);
  ppl_Polyhedron_add_constraint (ph, new_cstr);
  ppl_delete_Constraint (new_cstr);
}

/* Updates the scattering of all the PBBs under LST to be at the DEWEY
   number in the loop at depth LEVEL.  */

static inline void
lst_update_scattering_under (lst_p lst, int level, int dewey)
{
  int i;
  lst_p l;

  gcc_assert (lst && level >= 0 && dewey >= 0);

  if (LST_LOOP_P (lst))
    for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
      lst_update_scattering_under (l, level, dewey);
  else
    pbb_update_scattering (LST_PBB (lst), level, dewey);
}

/* Updates the scattering of all the PBBs under LST and in sequence
   with LST.  */

static inline void
lst_update_scattering_seq (lst_p lst)
{
  int i;
  lst_p l;
  lst_p father = LST_LOOP_FATHER (lst);
  int dewey = lst_dewey_number (lst);
  int level = lst_depth (lst);

  gcc_assert (lst && father && dewey >= 0 && level >= 0);

  for (i = dewey; VEC_iterate (lst_p, LST_SEQ (father), i, l); i++)
    lst_update_scattering_under (l, level, i);
}

/* Updates the all the scattering levels of all the PBBs under
   LST.  */

static inline void
lst_update_scattering (lst_p lst)
{
  int i;
  lst_p l;

  if (!lst || !LST_LOOP_P (lst))
    return;

  if (LST_LOOP_FATHER (lst))
    lst_update_scattering_seq (lst);

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
    lst_update_scattering (l);
}

/* Inserts LST1 before LST2 if BEFORE is true; inserts LST1 after LST2
   if BEFORE is false.  */

static inline void
lst_insert_in_sequence (lst_p lst1, lst_p lst2, bool before)
{
  lst_p father;
  int dewey;

  /* Do not insert empty loops.  */
  if (!lst1 || lst_empty_p (lst1))
    return;

  father = LST_LOOP_FATHER (lst2);
  dewey = lst_dewey_number (lst2);

  gcc_assert (lst2 && father && dewey >= 0);

  VEC_safe_insert (lst_p, heap, LST_SEQ (father), before ? dewey : dewey + 1,
		   lst1);
  LST_LOOP_FATHER (lst1) = father;
}

/* Replaces LST1 with LST2.  */

static inline void
lst_replace (lst_p lst1, lst_p lst2)
{
  lst_p father;
  int dewey;

  if (!lst2 || lst_empty_p (lst2))
    return;

  father = LST_LOOP_FATHER (lst1);
  dewey = lst_dewey_number (lst1);
  LST_LOOP_FATHER (lst2) = father;
  VEC_replace (lst_p, LST_SEQ (father), dewey, lst2);
}

/* Returns a copy of ROOT where LST has been replaced by a copy of the
   LSTs A B C in this sequence.  */

static inline lst_p
lst_substitute_3 (lst_p root, lst_p lst, lst_p a, lst_p b, lst_p c)
{
  int i;
  lst_p l;
  VEC (lst_p, heap) *seq;

  if (!root)
    return NULL;

  gcc_assert (lst && root != lst);

  if (!LST_LOOP_P (root))
    return new_lst_stmt (LST_PBB (root));

  seq = VEC_alloc (lst_p, heap, 5);

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (root), i, l); i++)
    if (l != lst)
      VEC_safe_push (lst_p, heap, seq, lst_substitute_3 (l, lst, a, b, c));
    else
      {
	if (!lst_empty_p (a))
	  VEC_safe_push (lst_p, heap, seq, copy_lst (a));
	if (!lst_empty_p (b))
	  VEC_safe_push (lst_p, heap, seq, copy_lst (b));
	if (!lst_empty_p (c))
	  VEC_safe_push (lst_p, heap, seq, copy_lst (c));
      }

  return new_lst_loop (seq);
}

/* Moves LST before LOOP if BEFORE is true, and after the LOOP if
   BEFORE is false.  */

static inline void
lst_distribute_lst (lst_p loop, lst_p lst, bool before)
{
  int loop_depth = lst_depth (loop);
  int depth = lst_depth (lst);
  int nb_loops = depth - loop_depth;

  gcc_assert (lst && loop && LST_LOOP_P (loop) && nb_loops > 0);

  lst_remove_from_sequence (lst);
  lst_insert_in_sequence (lst_create_nest (nb_loops, lst), loop, before);
}

/* Removes from LOOP all the statements before/after and including PBB
   if BEFORE is true/false.  Returns the negation of BEFORE when the
   statement PBB has been found.  */

static inline bool
lst_remove_all_before_including_pbb (lst_p loop, poly_bb_p pbb, bool before)
{
  int i;
  lst_p l;

  if (!loop || !LST_LOOP_P (loop))
    return before;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (loop), i, l);)
    if (LST_LOOP_P (l))
      {
	before = lst_remove_all_before_including_pbb (l, pbb, before);

	if (VEC_length (lst_p, LST_SEQ (l)) == 0)
	  {
	    VEC_ordered_remove (lst_p, LST_SEQ (loop), i);
	    free_lst (l);
	  }
	else
	  i++;
      }
    else
      {
	if (before)
	  {
	    if (LST_PBB (l) == pbb)
	      before = false;

	    VEC_ordered_remove (lst_p, LST_SEQ (loop), i);
	    free_lst (l);
	  }
	else if (LST_PBB (l) == pbb)
	  {
	    before = true;
	    VEC_ordered_remove (lst_p, LST_SEQ (loop), i);
	    free_lst (l);
	  }
	else
	  i++;
      }

  return before;
}

/* Removes from LOOP all the statements before/after and excluding PBB
   if BEFORE is true/false; Returns the negation of BEFORE when the
   statement PBB has been found.  */

static inline bool
lst_remove_all_before_excluding_pbb (lst_p loop, poly_bb_p pbb, bool before)
{
  int i;
  lst_p l;

  if (!loop || !LST_LOOP_P (loop))
    return before;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (loop), i, l);)
    if (LST_LOOP_P (l))
      {
	before = lst_remove_all_before_excluding_pbb (l, pbb, before);

	if (VEC_length (lst_p, LST_SEQ (l)) == 0)
	  {
	    VEC_ordered_remove (lst_p, LST_SEQ (loop), i);
	    free_lst (l);
	    continue;
	  }

	i++;
      }
    else
      {
	if (before && LST_PBB (l) != pbb)
	  {
	    VEC_ordered_remove (lst_p, LST_SEQ (loop), i);
	    free_lst (l);
	    continue;
	  }

	i++;

	if (LST_PBB (l) == pbb)
	  before = before ? false : true;
      }

  return before;
}

/* A SCOP is a Static Control Part of the program, simple enough to be
   represented in polyhedral form.  */
struct scop
{
  /* A SCOP is defined as a SESE region.  */
  void *region;

  /* Number of parameters in SCoP.  */
  graphite_dim_t nb_params;

  /* All the basic blocks in this scop that contain memory references
     and that will be represented as statements in the polyhedral
     representation.  */
  VEC (poly_bb_p, heap) *bbs;

  /* Original, transformed and saved schedules.  */
  lst_p original_schedule, transformed_schedule, saved_schedule;

  /* The context describes known restrictions concerning the parameters
     and relations in between the parameters.

  void f (int8_t a, uint_16_t b) {
    c = 2 a + b;
    ...
  }

  Here we can add these restrictions to the context:

  -128 >= a >= 127
     0 >= b >= 65,535
     c = 2a + b  */
  ppl_Pointset_Powerset_C_Polyhedron_t context;

  /* A hashtable of the data dependence relations for the original
     scattering.  */
  htab_t original_pddrs;

  /* True when the scop has been converted to its polyhedral
     representation.  */
  bool poly_scop_p;
};

#define SCOP_BBS(S) (S->bbs)
#define SCOP_REGION(S) ((sese) S->region)
#define SCOP_CONTEXT(S) (S->context)
#define SCOP_ORIGINAL_PDDRS(S) (S->original_pddrs)
#define SCOP_ORIGINAL_SCHEDULE(S) (S->original_schedule)
#define SCOP_TRANSFORMED_SCHEDULE(S) (S->transformed_schedule)
#define SCOP_SAVED_SCHEDULE(S) (S->saved_schedule)
#define POLY_SCOP_P(S) (S->poly_scop_p)

extern scop_p new_scop (void *);
extern void free_scop (scop_p);
extern void free_scops (VEC (scop_p, heap) *);
extern void print_generated_program (FILE *, scop_p);
extern void debug_generated_program (scop_p);
extern void print_scattering_function (FILE *, poly_bb_p);
extern void print_scattering_functions (FILE *, scop_p);
extern void debug_scattering_function (poly_bb_p);
extern void debug_scattering_functions (scop_p);
extern int scop_max_loop_depth (scop_p);
extern int unify_scattering_dimensions (scop_p);
extern bool apply_poly_transforms (scop_p);
extern bool graphite_legal_transform (scop_p);

/* Set the region of SCOP to REGION.  */

static inline void
scop_set_region (scop_p scop, void *region)
{
  scop->region = region;
}

/* Returns the number of parameters for SCOP.  */

static inline graphite_dim_t
scop_nb_params (scop_p scop)
{
  return scop->nb_params;
}

/* Set the number of params of SCOP to NB_PARAMS.  */

static inline void
scop_set_nb_params (scop_p scop, graphite_dim_t nb_params)
{
  scop->nb_params = nb_params;
}

/* Allocates a new empty poly_scattering structure.  */

static inline poly_scattering_p
poly_scattering_new (void)
{
  poly_scattering_p res = XNEW (struct poly_scattering);

  res->scattering = NULL;
  res->nb_local_variables = 0;
  res->nb_scattering = 0;
  return res;
}

/* Free a poly_scattering structure.  */

static inline void
poly_scattering_free (poly_scattering_p s)
{
  ppl_delete_Polyhedron (s->scattering);
  free (s);
}

/* Copies S and return a new scattering.  */

static inline poly_scattering_p
poly_scattering_copy (poly_scattering_p s)
{
  poly_scattering_p res = poly_scattering_new ();

  ppl_new_C_Polyhedron_from_C_Polyhedron (&(res->scattering), s->scattering);
  res->nb_local_variables = s->nb_local_variables;
  res->nb_scattering = s->nb_scattering;
  return res;
}

/* Saves the transformed scattering of PBB.  */

static inline void
store_scattering_pbb (poly_bb_p pbb)
{
  gcc_assert (PBB_TRANSFORMED (pbb));

  if (PBB_SAVED (pbb))
    poly_scattering_free (PBB_SAVED (pbb));

  PBB_SAVED (pbb) = poly_scattering_copy (PBB_TRANSFORMED (pbb));
}

/* Stores the SCOP_TRANSFORMED_SCHEDULE to SCOP_SAVED_SCHEDULE.  */

static inline void
store_lst_schedule (scop_p scop)
{
  if (SCOP_SAVED_SCHEDULE (scop))
    free_lst (SCOP_SAVED_SCHEDULE (scop));

  SCOP_SAVED_SCHEDULE (scop) = copy_lst (SCOP_TRANSFORMED_SCHEDULE (scop));
}

/* Restores the SCOP_TRANSFORMED_SCHEDULE from SCOP_SAVED_SCHEDULE.  */

static inline void
restore_lst_schedule (scop_p scop)
{
  if (SCOP_TRANSFORMED_SCHEDULE (scop))
    free_lst (SCOP_TRANSFORMED_SCHEDULE (scop));

  SCOP_TRANSFORMED_SCHEDULE (scop) = copy_lst (SCOP_SAVED_SCHEDULE (scop));
}

/* Saves the scattering for all the pbbs in the SCOP.  */

static inline void
store_scattering (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    store_scattering_pbb (pbb);

  store_lst_schedule (scop);
}

/* Restores the scattering of PBB.  */

static inline void
restore_scattering_pbb (poly_bb_p pbb)
{
  gcc_assert (PBB_SAVED (pbb));

  poly_scattering_free (PBB_TRANSFORMED (pbb));
  PBB_TRANSFORMED (pbb) = poly_scattering_copy (PBB_SAVED (pbb));
}

/* Restores the scattering for all the pbbs in the SCOP.  */

static inline void
restore_scattering (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    restore_scattering_pbb (pbb);

  restore_lst_schedule (scop);
}

/* For a given PBB, add to RES the scop context, the iteration domain,
   the original scattering when ORIGINAL_P is true, otherwise add the
   transformed scattering.  */

static inline void
combine_context_id_scat (ppl_Pointset_Powerset_C_Polyhedron_t *res,
			 poly_bb_p pbb, bool original_p)
{
  ppl_Pointset_Powerset_C_Polyhedron_t context;
  ppl_Pointset_Powerset_C_Polyhedron_t id;

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (res, original_p ?
     PBB_ORIGINAL_SCATTERING (pbb) : PBB_TRANSFORMED_SCATTERING (pbb));

  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&context, SCOP_CONTEXT (PBB_SCOP (pbb)));

  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&id, PBB_DOMAIN (pbb));

  /* Extend the context and the iteration domain to the dimension of
     the scattering: T|I|G.  */
  {
    ppl_dimension_type gdim, tdim, idim;

    ppl_Pointset_Powerset_C_Polyhedron_space_dimension (*res, &tdim);
    ppl_Pointset_Powerset_C_Polyhedron_space_dimension (context, &gdim);
    ppl_Pointset_Powerset_C_Polyhedron_space_dimension (id, &idim);

    if (tdim > gdim)
      ppl_insert_dimensions_pointset (context, 0, tdim - gdim);

    if (tdim > idim)
      ppl_insert_dimensions_pointset (id, 0, tdim - idim);
  }

  /* Add the context and the iteration domain to the result.  */
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (*res, context);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (*res, id);

  ppl_delete_Pointset_Powerset_C_Polyhedron (context);
  ppl_delete_Pointset_Powerset_C_Polyhedron (id);
}

#endif
