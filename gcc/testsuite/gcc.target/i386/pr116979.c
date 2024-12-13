/* PR target/116979 */
/* { dg-do compile } */
/* { dg-options "-O2 -mfma -fvect-cost-model=unlimited" } */
/* { dg-final { scan-assembler "vfmaddsub(?:132|213|231)pd" } } */
/* { dg-final { scan-assembler "vfmaddsub(?:132|213|231)ps" { target { ! ia32 } } } } */

struct S { __complex__ float f; };
struct T { __complex__ double f; };

struct S
foo (const struct S *a, const struct S *b)
{
  struct S r;
  r.f = a->f * b->f;
  return r;
}

struct T
bar (const struct T *a, const struct T *b)
{
  struct T r;
  r.f = a->f * b->f;
  return r;
}
