/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE (ssa)
foo (float a, float b)
{
  _Bool x;

 __BB(2):
  x_3 = a_1(D) __UNLT b_2(D);
  x_4 = a_1(D) __UNLE b_2(D);
  x_5 = a_1(D) __UNGT b_2(D);
  x_6 = a_1(D) __UNGE b_2(D);
  x_7 = a_1(D) __UNEQ b_2(D);
  x_8 = a_1(D) __UNORDERED b_2(D);
  x_9 = a_1(D) __ORDERED b_2(D);
  x_10 = a_1(D) __LTGT b_2(D);
  if (a_1(D) __UNEQ b_2(D))
    goto __BB4;
  else
    goto __BB3;

 __BB(3):
  goto __BB4;

 __BB(4):
  return;
}
