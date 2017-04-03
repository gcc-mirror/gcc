/* PR rtl-optimization/78911 */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-strict-aliasing -fno-omit-frame-pointer" } */
/* { dg-additional-options "-fPIC" { target fpic } } */
/* { dg-additional-options "-march=pentium-m" { target ia32 } } */

int a, b, d, e;
long long *c;

static int
foo (long long *x)
{
  return __sync_val_compare_and_swap (x, b, a);
}

void
bar (void)
{
  if (!c)
    return;
  e = foo (&c[d]);
}
