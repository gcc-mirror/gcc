/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test uses of parameter index in a function.
   TODO: cases with variadic functions,
         cases referring to variadic arguments.  */

#define NUMBER_ONE 1
#define NUMBER_TWO 2
#define NUMBER_THREE 3

void v_ptr(int *) {}
void macro_v_ptr(int *) {}

#pragma omp declare variant (v_ptr) match (construct={dispatch}) adjust_args (need_device_ptr: 1)
void b_ptr (int *) {}

#pragma omp declare variant (macro_v_ptr) match (construct={dispatch}) adjust_args (need_device_ptr: NUMBER_ONE)
void macro_b_ptr (int *) {}


void v_ptr_val(int *, int) {}
void macro_v_ptr_val(int *, int) {}

#pragma omp declare variant (v_ptr_val) match (construct={dispatch}) adjust_args (need_device_ptr: 1)
void b_ptr_val (int *, int) {}

#pragma omp declare variant (macro_v_ptr_val) match (construct={dispatch}) adjust_args (need_device_ptr: NUMBER_ONE)
void macro_b_ptr_val (int *, int) {}


void v_val_ptr(int, int *) {}
void macro_v_val_ptr(int, int *) {}

#pragma omp declare variant (v_val_ptr) match (construct={dispatch}) adjust_args (need_device_ptr: 2)
void b_val_ptr (int, int *) {}

#pragma omp declare variant (macro_v_val_ptr) match (construct={dispatch}) adjust_args (need_device_ptr: NUMBER_TWO)
void macro_b_val_ptr (int, int *) {}


void v_ptr_val_val(int *, int, int) {}
void macro_v_ptr_val_val(int *, int, int) {}

#pragma omp declare variant (v_ptr_val_val) match (construct={dispatch}) adjust_args (need_device_ptr: 1)
void b_ptr_val_val (int *, int, int) {}

#pragma omp declare variant (macro_v_ptr_val_val) match (construct={dispatch}) adjust_args (need_device_ptr: NUMBER_ONE)
void macro_b_ptr_val_val (int *, int, int) {}


void v_val_ptr_val(int, int *, int) {}
void macro_v_val_ptr_val(int, int *, int) {}

#pragma omp declare variant (v_val_ptr_val) match (construct={dispatch}) adjust_args (need_device_ptr: 2)
void b_val_ptr_val (int, int *, int) {}

#pragma omp declare variant (macro_v_val_ptr_val) match (construct={dispatch}) adjust_args (need_device_ptr: NUMBER_TWO)
void macro_b_val_ptr_val (int, int *, int) {}


void v_val_val_ptr(int, int, int *) {}
void macro_v_val_val_ptr(int, int, int *) {}

#pragma omp declare variant (v_val_val_ptr) match (construct={dispatch}) adjust_args (need_device_ptr: 3)
void b_val_val_ptr (int, int, int *) {}

#pragma omp declare variant (macro_v_val_val_ptr) match (construct={dispatch}) adjust_args (need_device_ptr: NUMBER_THREE)
void macro_b_val_val_ptr (int, int, int *) {}


void f(int *p0, int *p1, int *p2, int *p3, int *p4,
       int *p5, int *p6, int *p7, int *p8, int *p9,
       int *pA, int *pB)
{
  #pragma omp dispatch
  b_ptr (p0);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v_ptr \\(D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b_ptr_val (p1, 4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v_ptr_val \\(D\.\[0-9\]+, 4\\);" "gimple" } }  */
  #pragma omp dispatch
  b_val_ptr (4, p2);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v_val_ptr \\(4, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b_ptr_val_val (p3, 4, 4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p3, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v_ptr_val_val \\(D\.\[0-9\]+, 4, 4\\);" "gimple" } }  */
  #pragma omp dispatch
  b_val_ptr_val (4, p4, 4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v_val_ptr_val \\(4, D\.\[0-9\]+, 4\\);" "gimple" } }  */
  #pragma omp dispatch
  b_val_val_ptr (4, 4, p5);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p5, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v_val_val_ptr \\(4, 4, D\.\[0-9\]+\\);" "gimple" } }  */

  #pragma omp dispatch
  macro_b_ptr (p6);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p6, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "macro_v_ptr \\(D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  macro_b_ptr_val (p7, 4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p7, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "macro_v_ptr_val \\(D\.\[0-9\]+, 4\\);" "gimple" } }  */
  #pragma omp dispatch
  macro_b_val_ptr (4, p8);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p8, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "macro_v_val_ptr \\(4, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  macro_b_ptr_val_val (p9, 4, 4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p9, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "macro_v_ptr_val_val \\(D\.\[0-9\]+, 4, 4\\);" "gimple" } }  */
  #pragma omp dispatch
  macro_b_val_ptr_val (4, pA, 4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(pA, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "macro_v_val_ptr_val \\(4, D\.\[0-9\]+, 4\\);" "gimple" } }  */
  #pragma omp dispatch
  macro_b_val_val_ptr (4, 4, pB);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(pB, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "macro_v_val_val_ptr \\(4, 4, D\.\[0-9\]+\\);" "gimple" } }  */
}
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 12 "gimple" } }  */
