/* { dg-do compile } */
/* { dg-options "-O3" } */

/* a is not used after the comparison.  So we should use load and test
   here.  */

double gl;

void
bar (double a)
{
  if (a == 0.0)
    gl = 1;
}

/* { dg-final { scan-assembler "ltdbr\t" } } */
