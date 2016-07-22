/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-expand-details" } */

double
libcall_dep (double x, double y)
{
  double tem = x + y;
  return x * tem;
}

/* { dg-final { scan-rtl-dump-times "Swap operands" 1 "expand" } } */
