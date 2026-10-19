/* PR target/127773 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
/* { dg-final { scan-assembler "vzeroupper.*call\[ \t\]+_?f\\M" } } */

typedef char v32qi __attribute__ ((vector_size (32)));

static __attribute__ ((noinline)) int
f (void)
{
  __asm__ volatile ("pxor %%xmm0, %%xmm0" ::: "xmm0");
  return 0;
}

__attribute__ ((target ("avx2"))) int
g (v32qi *p)
{
  *p += *p;
  return f () + 1;
}
