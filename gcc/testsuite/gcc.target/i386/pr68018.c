/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O -mabi=ms -mstackrealign" } */

typedef float V __attribute__ ((vector_size (16)));

int fn1 (V * x)
{
  V a = *x;
  return a[0];
}
