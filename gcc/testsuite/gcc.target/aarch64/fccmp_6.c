/* { dg-do compile } */
/* { dg-options "-O2" } */

/* A trapping comparison must start the sequence.  Test every source
   position in a chain that is long enough for parallel reassociation.  */

#define DEF(N, EXPR) \
  int f##N (double a, double b, int c, int d, int e, int f, int g, int h) \
  { return (EXPR); }

DEF (0, (a < b) & (c < d) & (e < f) & (g < h))
DEF (1, (c < d) & (a < b) & (e < f) & (g < h))
DEF (2, (c < d) & (e < f) & (a < b) & (g < h))
DEF (3, (c < d) & (e < f) & (g < h) & (a < b))

/* { dg-final { scan-assembler-times {\tfcmpe\t} 4 } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 12 } } */
/* { dg-final { scan-assembler-times {\tcset\t} 4 } } */
/* { dg-final { scan-assembler-not {\tand\t} } } */
