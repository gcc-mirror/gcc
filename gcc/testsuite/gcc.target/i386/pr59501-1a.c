/* PR target/59501 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx -mno-accumulate-outgoing-args" } */

typedef double V __attribute__ ((vector_size (32)));

V
foo (double *x, unsigned *y)
{
  V r = { x[y[0]], x[y[1]], x[y[2]], x[y[3]] };
  return r;
}

/* Verify no dynamic realignment is performed.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*sp" } } */
/* And DRAP isn't needed either.  */
/* { dg-final { scan-assembler-not "r10" } } */
