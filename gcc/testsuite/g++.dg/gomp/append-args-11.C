/* PR c++/119602 */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include "append-args-omp-interop-t.h"

/* Check multiple instantiations with a dependent prefer_type value
   have the correct value.  */

typedef enum omp_interop_fr_t
{
  omp_ifr_cuda = 1,
  omp_ifr_cuda_driver = 2,
  omp_ifr_opencl = 3,
  omp_ifr_sycl = 4,
  omp_ifr_hip = 5,
  omp_ifr_level_zero = 6,
  omp_ifr_hsa = 7,
  omp_ifr_last = omp_ifr_hsa
} omp_interop_fr_t;

template<omp_interop_fr_t V>
struct FR {};

template<omp_interop_fr_t V, typename T2>
void v_dependent_fr(FR<V>, T2) { }

#pragma omp declare variant(v_dependent_fr) match(construct={dispatch}) \
					    append_args(interop(target, \
							prefer_type(V)))
template<omp_interop_fr_t V>
void b_dependent_fr(FR<V>) { }


template<typename T, typename T2>
void v_cuda(T, T2) { }

#pragma omp declare variant(v_cuda) match(construct={dispatch}) \
				    append_args(interop(target, \
						prefer_type(omp_ifr_cuda_driver)))
template<typename T>
void b_cuda(T) { }


template<typename T, typename T2>
void v_hip(T, T2) { }

#pragma omp declare variant(v_hip) match(construct={dispatch}) \
				   append_args(interop(target, \
					       prefer_type(omp_ifr_hip)))
template<typename T>
void b_hip(T) { }


template<typename T, typename T2>
void v_hsa(T, T2) { }

#pragma omp declare variant(v_hsa) match(construct={dispatch}) \
				   append_args(interop(target, \
						       prefer_type(omp_ifr_hsa)))
template<typename T>
void b_hsa(T) { }

void f ()
{
  #pragma omp dispatch
  b_dependent_fr (FR<omp_ifr_cuda_driver>());

  #pragma omp dispatch
  b_dependent_fr (FR<omp_ifr_hip>());

  #pragma omp dispatch
  b_dependent_fr (FR<omp_ifr_level_zero>());

  #pragma omp dispatch
  b_dependent_fr (FR<omp_ifr_hsa>());

  #pragma omp dispatch
  b_cuda (0);

  #pragma omp dispatch
  b_hip (0);

  #pragma omp dispatch
  b_hsa (0);
}

/* "\\\[" and "\\\]" matches a literal '[' and ']',
   "\\\\" matches a literal '\' in case anyone is wondering.  */
/* omp_ifr_cuda_driver */
/* { dg-final { scan-tree-dump-times "pref_type\.\[0-9\]+\\\[0\\\] = \"\\\\x80\\\\x02\\\\x80\\\\x00\";" 2 "gimple" } }  */
/* omp_ifr_hip */
/* { dg-final { scan-tree-dump-times "pref_type\.\[0-9\]+\\\[0\\\] = \"\\\\x80\\\\x05\\\\x80\\\\x00\";" 2 "gimple" } }  */
/* omp_ifr_level_zero */
/* { dg-final { scan-tree-dump-times "pref_type\.\[0-9\]+\\\[0\\\] = \"\\\\x80\\\\x06\\\\x80\\\\x00\";" 1 "gimple" } }  */
/* omp_ifr_hsa */
/* { dg-final { scan-tree-dump-times "pref_type\.\[0-9\]+\\\[0\\\] = \"\\\\x80\\\\x07\\\\x80\\\\x00\";" 2 "gimple" } }  */
