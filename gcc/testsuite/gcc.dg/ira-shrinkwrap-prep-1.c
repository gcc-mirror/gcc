/* { dg-do compile { target { { { i?86-*-* x86_64-*-* } && lp64 } || { { powerpc*-*-* && lp64 } || { arm_nothumb || { aarch64*-*-* && lp64 } } } } } } */
/* { dg-options "-O3 -fdump-rtl-ira -fdump-rtl-pro_and_epilogue -fno-ipa-ra"  } */

long __attribute__((noinline, noclone))
foo (long a)
{
  return a + 5;
}

static long g __attribute__ ((used));

long __attribute__((noinline, noclone))
bar (long a)
{
  long r;

  if (a)
    {
      r = foo (a);
      g = r + a;
    }
  else
    r = a+1;
  return r;
}

/* { dg-final { scan-rtl-dump "Will split live ranges of parameters" "ira" } } */
/* { dg-final { scan-rtl-dump "Split live-range of register" "ira" { xfail *-*-* } } } */
/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" { xfail powerpc*-*-* } } } */
