/* { dg-do run } */
/* { dg-additional-options "-fgimple" } */

int __attribute__((noipa))
__GIMPLE(ssa,startwith("dom")) bar(int cond, int val)
{
  int i;
  _Bool _7;

__BB(3):
  if (0 != 0)
    goto __BB6;
  else
    goto __BB2;

__BB(2):
  if (cond_5(D) != 0)
    goto __BB4;
  else
    goto __BB5;

__BB(4):
  i_6 = val_2(D);
  _7 = val_2(D) > 0;
  i_1 = _7 ? i_6 : 0;
  goto __BB5;

__BB(5):
  i_3 = __PHI (__BB4: i_1, __BB2: 0);
  return i_3;

__BB(6):
  i_4 = 1;
  i_9 = 2;
  goto __BB2;
}

int main()
{
  if (bar (1, 1) != 1)
    __builtin_abort ();
  return 0;
}
