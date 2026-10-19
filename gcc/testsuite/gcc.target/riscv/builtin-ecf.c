/* { dg-do compile } */
/* { dg-options "-march=rv32i_xcvalu -mabi=ilp32 -O2 -fdump-tree-optimized" } */

/* (most) builtins are pure, so do not mutate memory, hence allow moving loads/stores across them */
int foo (int *ptr, int a, int b)
{
  int r = *ptr;
  int s = __builtin_riscv_cv_alu_min (a, b);
  r += *ptr;
  return s + r;
}
/* The loads should be merged  */
/* { dg-final { scan-tree-dump-times { = \*ptr} 1 "optimized" } } */
