/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE () foo ()
{
  int a;

bb_2:
  if (a > 4)
    goto bb_3;
  else
    goto bb_4;

bb_3:
  a_2 = 10;
  goto bb_5;

bb_4:
  a_3 = 20;

bb_5:
  a_1 = __PHI (bb_3: a_2, bb_4: a_3);
  a_4 = a_1 + 4;

return;
}

