/* { dg-do compile { target { { { i?86-*-* x86_64-*-* } && lp64 } || { arm_thumb2 } } } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue"  } */

/*
Our new threader is threading things a bit too early, and causing the
testcase in gcc.dg/shrink-wrap-loop.c to fail.

  The gist is this BB inside a loop:

  <bb 6> :
  # p_2 = PHI <p2_6(D)(2), p_12(5)>
  if (p_2 != 0B)
    goto <bb 3>; [INV]
  else
    goto <bb 7>; [INV]

Our threader can move this check outside of the loop (good).  This is
done before branch probabilities are calculated and causes the probs
to be calculated as:

<bb 2> [local count: 216361238]:
  if (p2_6(D) != 0B)
    goto <bb 7>; [54.59%]
  else
    goto <bb 6>; [45.41%]

Logically this seems correct to me.  A simple check outside of a loop
should slightly but not overwhelmingly favor a non-zero value.

Interestingly however, the old threader couldn't get this, but the IL
ended up identical, albeit with different probabilities.  What happens
is that, because the old code could not thread this, the p2 != 0 check
would remain inside the loop and probs would be calculated thusly:

  <bb 6> [local count: 1073741824]:
  # p_2 = PHI <p2_6(D)(2), p_12(5)>
  if (p_2 != 0B)
    goto <bb 3>; [94.50%]
  else
    goto <bb 7>; [5.50%]

Then when the loop header copying pass ("ch") shuffled things around,
the IL would end up identical to my early threader code, but with the
probabilities would remain as 94.5/5.5.

The above discrepancy causes the RTL ifcvt pass to generate different
code, and by the time we get to the shrink wrapping pass, things look
sufficiently different such that the legacy code can actually shrink
wrap, whereas our new code does not.

IMO, if the loop-ch pass moves conditionals outside of a loop, the
probabilities should be adjusted, but that does mean the shrink wrap
won't happen for this contrived testcase.
 */

int foo (int *p1, int *p2);

int
test (int *p1, int *p2)
{
  int *p;

  for (p = p2; p != 0; p++)
    {
      if (!foo (p, p1))
        return 0;
    }

  return 1;
}
/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" { xfail *-*-* } } } */
