/* PR target/63620 */
/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -fpic -mfpmath=sse -msse" } */

static const __float128 cf = 0.1E+30Q;

typedef __float128 (*func)(__float128 x);

__float128
test (__float128 x, int p, func f)
{
  x = f (x);
  if (p)
    x = f (cf);
  return x;
}

/* { dg-final { scan-assembler "get_pc_thunk" { xfail { *-*-solaris* && { ! gld } } } } } */
