/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef float v2sf __attribute__ ((vector_size (8)));
typedef double v2df __attribute__ ((vector_size (16)));

/*
** extendv2sfv2df2:
**     vmrhf	%v24,%v24,%v24
**     vldeb	%v24,%v24
**     br	%r14
*/

v2df extendv2sfv2df2 (v2sf x)
{
  return __builtin_convertvector (x, v2df);
}
