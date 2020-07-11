/* { dg-do compile { target fpic } } */
/* { dg-options "-O3 -fPIC -fno-asynchronous-unwind-tables" } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("default,avx2")))
static int
f2(int *p)
{
  asm volatile ("" :: "r"(p) : "memory");
  return *p;
}

__attribute__((target_clones("default,avx2")))
int
g2(int *p)
{
  return f2(p);
}

/* { dg-final { scan-assembler "g2.default.1:\n\tjmp\tf2.default.1\n" } } */
/* { dg-final { scan-assembler "g2.avx2.0:\n\tjmp\tf2.avx2.0\n" } } */
