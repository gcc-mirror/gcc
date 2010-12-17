/* { dg-options "-Os -march=armv5te -mthumb" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler "pop.*r1.*pc" } } */
/* { dg-final { scan-assembler-not "sub\[\\t \]*sp,\[\\t \]*sp" } } */
/* { dg-final { scan-assembler-not "add\[\\t \]*sp,\[\\t \]*sp" } } */

extern void bar(int*);
int foo()
{
  int x;
  bar(&x);
  return x;
}
