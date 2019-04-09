/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */

/* Check code generation for one's complement version of abs */

int onecmplabs(int x)
{
  if (x < 0)
    x = ~x;
  return x;
}

/* { dg-final { scan-rtl-dump "succeeded through noce_try_abs" "ce1" } } */
