/* A masked load is a read even though its expander requires a MEM.  */

/* { dg-do compile } */
/* { dg-options "-O -fgimple -mavx2 -mno-avx512f -fnon-call-exceptions -fdump-tree-optimized -fdump-rtl-expand" } */

typedef int v4si __attribute__ ((vector_size (16)));

extern const v4si values;

v4si __GIMPLE (ssa)
load (v4si mask)
{
  v4si result;

  __BB(2):
  result_2 = .MASK_LOAD (&values, _Literal (v4si *) 128,
			mask_1(D),
			_Literal (v4si) { 0, 0, 0, 0 });
  return result_2;
}

/* { dg-final { scan-tree-dump {\.MASK_LOAD} "optimized" } } */
/* { dg-final { scan-rtl-dump-times {\(mem/u/c:V4SI} 1 "expand" } } */
