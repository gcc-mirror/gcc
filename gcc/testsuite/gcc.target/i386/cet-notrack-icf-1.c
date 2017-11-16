/* Verify nocf_check functions are not ICF optimized.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "endbr" } } */
/* { dg-final { scan-assembler-not "fn3:" } } */
/* { dg-final { scan-assembler "set\[ \t]+fn2,fn1" } } */
/* { dg-final { scan-assembler "set\[ \t]+fn3,fn1" } } */

static __attribute__((noinline)) int
fn1 (int x)
{
  return x + 12;
}

static __attribute__((noinline)) int
fn2 (int x)
{
  return x + 12;
}

static __attribute__((noinline, nocf_check)) int
fn3 (int x)
{ /* { dg-warning "'nocf_check' attribute ignored. Use -fcf-protection option to enable it" } */
  return x + 12;
}

int
fn4 (int x)
{
  return fn1 (x) + fn2 (x) + fn3 (x);
}
