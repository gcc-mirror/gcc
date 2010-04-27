/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */

extern void bar(int*);
int foo()
{
  int x;
  bar(&x);
  return x;
}

/* { dg-final { scan-assembler-not "sub\[\\t \]*sp,\[\\t \]*sp," } } */
