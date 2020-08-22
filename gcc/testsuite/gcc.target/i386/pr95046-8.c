/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse3" } */


typedef float __v2sf __attribute__ ((__vector_size__ (8)));

float
foo (__v2sf a)
{
  return a[0] + a[1];
}

/* { dg-final { scan-assembler "\tv?haddps" } } */

float
bar (__v2sf a)
{
  return a[0] - a[1];
}

/* { dg-final { scan-assembler "\tv?hsubps" } } */
