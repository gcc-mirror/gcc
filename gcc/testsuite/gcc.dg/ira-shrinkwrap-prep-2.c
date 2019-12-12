/* { dg-do compile { target { { { i?86-*-* x86_64-*-* } && lp64 } || { { powerpc*-*-* && lp64 } || { arm_nothumb || { aarch64*-*-* && lp64 } } } } } } */
/* { dg-options "-O3 -fdump-rtl-ira -fdump-rtl-pro_and_epilogue -fno-ipa-ra"  } */

long __attribute__((noinline, noclone))
foo (long a)
{
  return a + 5;
}

static long g;

long __attribute__((noinline, noclone))
bar (long a)
{
  long r;

  if (a)
    {
      r = a;
      while (r < 500)
	if (r % 2)
	  r = foo (r);
	else
	  r = foo (r+1);
      g = r + a;
    }
  else
    r = g+1;
  return r;
}

/* { dg-final { scan-rtl-dump "Will split live ranges of parameters" "ira" } } */
/* { dg-final { scan-rtl-dump "Split live-range of register" "ira" { xfail *-*-* } } } */
/* XFAIL due to PR70681.  */ 
/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" { xfail arm*-*-* powerpc*-*-* } } } */
