/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx512f" } */

union flt
{
  _Float16 flt;
  short s;
};

_Float16
foo (union flt x)
{
  return x.flt;
}

/* { dg-final { scan-assembler {(?n)pinsrw[\t ].*%xmm0} } } */
