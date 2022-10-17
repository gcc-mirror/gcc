/* { dg-do assemble { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

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
