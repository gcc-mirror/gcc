/* PR tree-optimization/59523 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

int *
foo (int a, int *b, int *c, int *d)
{
  int i, *r = __builtin_alloca (a * sizeof (int));
  __builtin_memcpy (r, d, a * sizeof (int));
  for (i = 0; i < 64; i++)
    c[i] += b[i];
  for (i = 0; i < a; i++)
    if (r[i] == 0)
      r[i] = 1;
  return r;
}

/* { dg-prune-output "-Wreturn-local-addr" } */
