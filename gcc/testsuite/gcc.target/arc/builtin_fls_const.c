/* Test that const attribute enables CSE optimization for ARC builtins.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int test_fls_cse(int x)
{
  /* Two calls to the same const builtin with same argument should
     be optimized to a single call plus a multiply-by-2 operation.  */
  int a = __builtin_arc_fls(x);
  int b = __builtin_arc_fls(x);
  return a + b;
}

int test_ffs_cse(int x)
{
  /* Same pattern for __builtin_arc_ffs.  */
  int a = __builtin_arc_ffs(x);
  int b = __builtin_arc_ffs(x);
  return a + b;
}

int test_norm_cse(int x)
{
  /* Same pattern for __builtin_arc_norm.  */
  int a = __builtin_arc_norm(x);
  int b = __builtin_arc_norm(x);
  return a + b;
}

/* { dg-final { scan-assembler-times "fls\\s+" 1 } } */
/* { dg-final { scan-assembler-times "ffs\\s+" 1 } } */
/* { dg-final { scan-assembler-times "norm\\s+" 1 } } */

/* Verify that the result is multiplied by 2 using left shift.  */
/* { dg-final { scan-assembler "asl_s\\s+.*,.*,1" } } */
