/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O0 -fdump-tree-optimized" } */

int george;
int ringo;

__attribute__((transaction_callable))
void foo()
{
  ringo=666;
  __transaction_atomic {
      george=999;
  }
}

/* There should only be 2 instrumented writes to GEORGE: one in FOO,
   and one in the transactional clone to FOO.  There should NOT be
   more than one instrumented write to GEORGE in the clone of
   FOO.  */
/* { dg-final { scan-tree-dump-times "ITM_WU\[0-9\] \\(&george," 2 "optimized" } } */

