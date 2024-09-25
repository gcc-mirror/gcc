/* { dg-do compile } */
/* { dg-options "-march=rv32imac -mabi=ilp32 -O2 -fdump-rtl-ira" } */

double foo (double a)
{
  if (a < 0.0)
    return a + 1.0;
  else if (a > 16.0)
    return a - 3.0;
  else if (a < 300.0)
    return a - 30.0;
  else
    return a;
}

/* { dg-final { scan-rtl-dump-not "memory is more profitable" "ira" } } */
