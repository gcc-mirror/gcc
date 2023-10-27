/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-mno-avx -O3" } */

void f (double *x)
{
  for (int i = 0; i != 8; ++i)
    x[i] *= 2;
}

#pragma GCC target("avx")

void g (double *x)
{
  for (int i = 0; i != 8; ++i)
    x[i] *= 2;
}

/* Make sure the target pragma affected only g() and not also f().  */
/* { dg-final { scan-assembler-times vzeroupper 1 } } */
