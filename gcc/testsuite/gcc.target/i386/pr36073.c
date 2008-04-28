/* { dg-do compile } */
/* { dg-options "-O -march=core2 -mfpmath=sse,387 -ffast-math" } */

extern double log (double x);
extern int f (void);

double cached_value;

void g (void)
{
  cached_value = log (f ());
}
