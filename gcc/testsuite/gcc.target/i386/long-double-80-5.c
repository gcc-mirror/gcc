/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-64" } */

__float80
foo (__float80 x)
{
  return x * x;
}

/* { dg-final { scan-assembler "fldt" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*_?__multf3" } } */
