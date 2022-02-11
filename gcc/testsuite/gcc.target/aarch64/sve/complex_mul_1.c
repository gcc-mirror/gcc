/* { dg-options "-O2 -fgimple -fdump-tree-optimized" } */

void __GIMPLE
foo (__SVFloat64_t x, __SVFloat64_t y, __SVFloat64_t *res1,
     __SVFloat64_t *res2)
{
  __SVFloat64_t a1;
  __SVFloat64_t a2;

  a1 = .COMPLEX_MUL (x, y);
  a2 = .COMPLEX_MUL (y, x);
  __MEM<__SVFloat64_t> (res1) = a1;
  __MEM<__SVFloat64_t> (res2) = a2;
}

/* { dg-final { scan-tree-dump-times {\.COMPLEX_MUL} 1 "optimized" } } */
