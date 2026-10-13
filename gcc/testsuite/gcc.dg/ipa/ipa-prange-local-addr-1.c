/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp-details" } */

/* A prange can carry the address of an object.  The address of an automatic
   is gimple_min_invariant inside its own function but not ip-invariant, so
   it must not reach a callee through a jump function.  This test confirms the
   equivalent fix for PR125857 for globals and params is present.  */

int sink;

static __attribute__((noinline)) int
walk (int x, int *flagp)
{
  if (x > 10) { *flagp = 1; return 1; }
  if (x < -10) { *flagp = 2; return 2; }
  return 0;
}

int
top (int a, int b)
{
  int flag = 0;
  int r = walk (a, &flag);
  if (r)
    return r;
  r = walk (b, &flag);
  if (r)
    return r;
  return flag;
}

/* { dg-final { scan-ipa-dump "Dropping non ipa invariant points-to" "cp" } } */
