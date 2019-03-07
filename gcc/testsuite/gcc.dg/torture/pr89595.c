/* { dg-do run } */
/* { dg-additional-options "-fgimple" } */

int __attribute__((noipa))
__GIMPLE(startwith("dom")) bar(int cond, int val)
{
  int i;

  if (0 != 0)
    goto bb_6;
  else
    goto bb_2;

bb_2:
  if (cond_5(D) != 0)
    goto bb_4;
  else
    goto bb_5;

bb_4:
  i_6 = val_2(D);
  i_1 = val_2(D) > 0 ? i_6 : 0;

bb_5:
  i_3 = __PHI (bb_4: i_1, bb_2: 0);
  return i_3;

bb_6:
  i_4 = 1;
  i_9 = 2;
  goto bb_2;
}

int main()
{
  if (bar (1, 1) != 1)
    __builtin_abort ();
  return 0;
}
