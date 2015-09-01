/* { dg-do compile { target aarch64*-*-* x86_64-*-* } } */
/* { dg-options "-fdump-rtl-ce1 -O2" } */

int
foo (int x)
{
  return x > 100 ? x - 2 : x - 1;
}

/* { dg-final { scan-rtl-dump "3 true changes made" "ce1" } } */
