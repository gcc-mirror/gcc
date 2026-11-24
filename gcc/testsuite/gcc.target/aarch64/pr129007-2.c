/* PR tree-optimization/129007 */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -march=armv8.3-a" } */

/* c - a * b with the accumulator not loaded with LD2: the loaded x[i]
   and y[i] go straight into FCMLA, without permuting their halves.  */

void
f (_Complex double *restrict c, _Complex double *restrict x,
   _Complex double *restrict y, int n, int s)
{
  for (int i = 0; i < n; i++)
    c[i * s] -= x[i] * y[i];
}

/* { dg-final { scan-assembler-times {\tfcmla\t} 2 } } */
/* { dg-final { scan-assembler-not {\tdup\t} } } */
/* { dg-final { scan-assembler-not {\text\t} } } */
