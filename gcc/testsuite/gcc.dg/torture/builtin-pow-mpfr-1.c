/* Version 2.2.0 of MPFR had bugs in pow underflow/overflow.  This
   test checks to see if that buggy version was installed.  The
   problem is NOT fixed with the version 2.2.0 "cumulative patch".
   However it is fixed in version 2.2.1 and presumably later MPFR
   versions.

   Origin: Kaveh R. Ghazi 12/17/2006.  */

/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

extern double testit()
{
  /* This underflows and therefore gcc should not fold it.  */
  return __builtin_pow (0.11, 1.0e38);
}

/* { dg-final { scan-tree-dump "pow" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
