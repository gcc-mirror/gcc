/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -ftree-vectorize" } */

short a, b, c, d;

void f(void)
{
  short e;

  for(; e; e++)
    for(; b; b++);

  for(d = 0; d < 4; d++)
    a ^= (e ^= 1) || c ? : e;
}
