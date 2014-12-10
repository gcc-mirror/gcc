/* { dg-do compile {target sparc64*-*-* aarch64*-*-* i?86-*-* x86_64-*-* powerpc64*-*-*} } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdump-rtl-combine-all" } */

typedef long long int int64_t;

int64_t
foo (int64_t a)
{
  return (~a) >> 63;
}

/* The combine phase will try to combine not & ashiftrt, and
   combine_simplify_rtx should transform (ashiftrt (not x) 63)
   to (not (ashiftrt x 63)) and then to (neg (ge x 0)). We look for
   the *attempt* to match this RTL pattern, regardless of whether an
   actual insn may be found on the platform.  */
/* { dg-final { scan-rtl-dump "\\(neg:DI \\(ge:DI" "combine" } } */
/* { dg-final { cleanup-rtl-dump "combine" } } */
