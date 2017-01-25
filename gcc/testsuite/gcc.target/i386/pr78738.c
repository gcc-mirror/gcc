/* PR middle-end/78738 */
/* { dg-do compile } */
/* { dg-options "-O -std=c99 -ffast-math -mfpmath=387" } */

double round (double);

int foo (double a)
{
  return round (a);
}
