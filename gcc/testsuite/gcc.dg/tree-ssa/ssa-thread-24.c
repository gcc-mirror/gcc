/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-thread1-details" } */

/* Test that we can thread 4 -> 5 -> 7.  */

int result;
int shouldnt_happen_from_bb4;

void __GIMPLE (startwith ("thread1")) foo (int arg1, int arg2)
{
  int v1;
  int _14;
  unsigned int _15;
  unsigned int _16;

 bb_2:
  if (arg1 == arg2)
    goto bb_3;
  else
    goto bb_4;

 bb_3:
  result = 1;
  goto bb_5;

 bb_4:
  result = 2;

 bb_5:
  v1_595 = __PHI (bb_3: arg1, bb_4: 0);
  _14 = v1_595 * 3600;
  _15 = (unsigned int) _14;
  _16 = _15 / 60U;
  if (_16 > 389U)
    goto bb_6;
  else
    goto bb_7;

 bb_6:
  shouldnt_happen_from_bb4 = 0;
  goto bb_8;

 bb_7:
  result = 3;

 bb_8:
  return;
}

/* { dg-final { scan-tree-dump-times "Registering FSM jump thread: \\(4, 5\\)" 1 "thread1" } } */
