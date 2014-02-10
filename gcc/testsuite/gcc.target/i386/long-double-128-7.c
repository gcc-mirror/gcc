/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-64" } */

__float128
foo (__float128 x)
{
  return x * x;
}

/* { dg-final { scan-assembler-not "fldt" } } */
/* { dg-final { scan-assembler "call\[\\t \]*_?__multf3" } } */
