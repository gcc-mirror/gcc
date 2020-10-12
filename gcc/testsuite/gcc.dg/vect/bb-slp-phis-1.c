/* From gcc.c-torture/execute/loop-13.c  */
/* { dg-do compile } */
/* { dg-additional-options "-march=cascadelake" { target x86_64-*-* i?86-*-* } } */
#define TYPE long

void
scale (TYPE *alpha, TYPE *x, int n)
{
  int i, ix;

  if (*alpha != 1)
    for (i = 0, ix = 0; i < n; i++, ix += 2)
      {
	TYPE tmpr, tmpi;
	tmpr = *alpha * x[ix];
	tmpi = *alpha * x[ix + 1];
	x[ix] = tmpr;
	x[ix + 1] = tmpi;
      }
}
