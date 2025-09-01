/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=m2 -fdump-tree-optimized" } */

void foo(int * restrict a, int *b, int *c)
{
    for (int i=0;i<32;++i)
      a[i] = b[i] + c[i];
}

/* Make sure -mrvv-max-lmul still constraint the auto vectorizer for VLS
   types.  */
/* { dg-final { scan-assembler {vsetivli\s+zero,8,e32,m2,t[au],m[au]} } } */
