/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-expand-details" } */

double
libcall_dep (double x, double y)
{
  return x * (x + y);
}

/* { dg-final { scan-rtl-dump-times "Swap operands" 1 "expand" } } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
