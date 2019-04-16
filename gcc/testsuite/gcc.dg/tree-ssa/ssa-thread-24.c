/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-thread1-details" } */

/* Test that we can thread 4 -> 5 -> 7.  */

int result;
int shouldnt_happen_from_bb4;

void __GIMPLE (ssa, startwith ("thread1")) foo (int xarg1, int xarg2)
{
  int v1;
  int _14;
  unsigned int _15;
  unsigned int _16;
  int arg1;
  int arg2;

 __BB(2):
  arg1_597 = xarg1_9(D);
  arg2_598 = xarg2_10(D);
  if (arg1_597 == arg2_598)
    goto __BB3;
  else
    goto __BB4;

 __BB(3):
  result = 1;
  goto __BB5;

 __BB(4):
  result = 2;
  goto __BB5;

 __BB(5):
  v1_595 = __PHI (__BB3: arg1_597, __BB4: 0);
  _14 = v1_595 * 3600;
  _15 = (unsigned int) _14;
  _16 = _15 / 60U;
  if (_16 > 389U)
    goto __BB6;
  else
    goto __BB7;

 __BB(6):
  shouldnt_happen_from_bb4 = 0;
  goto __BB8;

 __BB(7):
  result = 3;

 __BB(8):
  return;
}

/* { dg-final { scan-tree-dump-times "Registering FSM jump thread: \\(4, 5\\)" 1 "thread1" } } */
