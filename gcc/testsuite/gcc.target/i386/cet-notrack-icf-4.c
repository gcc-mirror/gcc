/* Verify nocf_check function calls are not ICF optimized.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler "endbr" } } */
/* { dg-final { scan-assembler "fn2:" } } */
/* { dg-final { scan-assembler "set\[ \t]+fn3,fn1" { target { ! *-*-darwin* } } } } */

int (*foo)(int);

typedef int (*type1_t) (int) __attribute__ ((nocf_check));
typedef int (*type2_t) (int);

static __attribute__((noinline)) int
fn1 (int x)
{
  return ((type2_t)foo)(x + 12);
}

static __attribute__((noinline)) int
fn2 (int x)
{
  return ((type1_t)foo)(x + 12);
}

static __attribute__((noinline)) int
fn3 (int x)
{
  return ((type2_t)foo)(x + 12);
}

int
fn4 (int x)
{
  return fn1 (x) + fn2 (x) + fn3 (x);
}
