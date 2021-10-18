/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=387 -fexcess-precision=16" } */
/* { dg-excess-errors "'-fexcess-precision=16' is not compatible with '-mfpmath=387'" } */
_Float16
foo (_Float16 a, _Float16 b)
{
  return a + b;/* { dg-error "'-fexcess-precision=16' is not compatible with '-mfpmath=387'" } */
}

