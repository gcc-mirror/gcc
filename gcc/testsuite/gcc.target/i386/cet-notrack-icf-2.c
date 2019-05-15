/* Verify nocf_check functions are not ICF optimized.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler "endbr" } } */
/* { dg-final { scan-assembler "fn3:" } } */
/* { dg-final { scan-assembler "set\[ \t]+fn2,fn1" { target { ! *-*-darwin* } } } } */

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
{
  return x + 12;
}

int
fn4 (int x)
{
  return fn1 (x) + fn2 (x) + fn3 (x);
}
