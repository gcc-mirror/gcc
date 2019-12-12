/* { dg-do compile } */
/* { dg-options "-O3" } */

/* Check that simple loop is not fully unrolled.  */

void
fully_peel_me (double *x)
{
  for (int i = 0; i < 5; i++)
    x[i] = x[i] * 2;
}

/* { dg-final { scan-assembler-times {\tb[.a-z]+\t} 1 } } */
