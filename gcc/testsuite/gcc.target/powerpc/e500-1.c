/* Test functioning of command option -mno-isel */
/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-options "-O2 -mno-isel" } */

/* { dg-final { scan-assembler-not "isel" } } */

int
foo (int x, int y)
{
  if (x < y)
    return x;
  else
    return y;
}
