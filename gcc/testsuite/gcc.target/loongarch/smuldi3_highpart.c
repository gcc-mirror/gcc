/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -fdump-rtl-expand-all" } */

typedef int TI __attribute ((mode(TI)));
typedef int DI __attribute__((mode(DI)));

DI
test (DI x, DI y)
{
  return ((TI)x * y) >> 64;
}

/* { dg-final { scan-rtl-dump "highparttmp" "expand" } } */
