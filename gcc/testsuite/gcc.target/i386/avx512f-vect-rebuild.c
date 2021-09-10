/* { dg-do compile } */
/* { dg-options "-O -mavx512vl -mavx512dq -fno-tree-forwprop" } */

typedef double v2df __attribute__ ((__vector_size__ (16)));
typedef double v4df __attribute__ ((__vector_size__ (32)));

v2df h (v4df x)
{
  v2df xx = { x[2], x[3] };
  return xx;
}

v4df f2 (v4df x)
{
  v4df xx = { x[0], x[1], x[2], x[3] };
  return xx;
}

/* { dg-final { scan-assembler-not "unpck" } } */
/* { dg-final { scan-assembler-not "valign" } } */
/* { dg-final { scan-assembler-times "\tv?extract(?:f128|f64x2)\[ \t\]" 1 } } */
