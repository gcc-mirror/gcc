/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fdump-rtl-combine-all" } */

/* This testcase checks if combine tries to combine sequences where the last
   insn has a clobber of a reg, and a previous insn sets that reg.

   In this case, we have three insns

   (set flags (compare a b))
   (set tmp (eq flags 0))
   (parallel [(set dst (neg tmp))
	      (clobber flags)])

   Previously, combine would not try the three-insn combination because of
   the set and clobber of flags.  Now it does.  Test that.  */


int f(int a, int b) { return -(a == b); }

/* This regexp works for reg parameters as well as mem parameters.  */
/* { dg-final { scan-rtl-dump {neg:SI[^:]*eq:SI[^:]*:SI} "combine" } } */
