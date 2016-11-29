/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int a;
void __GIMPLE () foo ()
{
  int b;
  int c;

bb_2:
  b = a;
  if (b > 3)
    goto bb_3;
  else
    goto bb_4;

bb_3:
  b = c + 4;
  goto bb_5;

bb_4:
  b = b + 1;
  goto bb_5;

bb_5:
  a = b;
  return;
}
