/* { dg-do compile {target arm*-*-* i?86-*-* powerpc-*-* sparc-*-*} } */
/* { dg-options "-O2 -fdump-rtl-combine-all" } */

typedef long int32_t;

int32_t
foo (int32_t a)
{
  return (~a) >> 31;
}

/* The combine phase will try to combine not & ashiftrt, and
   combine_simplify_rtx should transform (ashiftrt (not x) 31)
   to (not (ashiftrt x 63)) and then to (neg (ge x 0)). We look for
   the *attempt* to match this RTL pattern, regardless of whether an
   actual insn may be found on the platform.  */
/* { dg-final { scan-rtl-dump "\\(neg:SI \\(ge:SI" "combine" } } */
/* { dg-final { cleanup-rtl-dump "combine" } } */
