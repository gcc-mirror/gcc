/* { dg-options "-Os -mthumb" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler-not "sub\[\\t \]*sp,\[\\t \]*sp" } } */
/* { dg-final { scan-assembler-not "add\[\\t \]*sp,\[\\t \]*sp" } } */

/* Here, we test that if there's a pop of r[4567] in the epilogue,
   add sp,sp,#12 is removed and replaced by three additional pops
   of lower-numbered regs.  */

extern void bar(int*);

int t1, t2, t3, t4, t5;
int foo()
{
  int i,j,k,x = 0;
  for (i = 0; i < t1; i++)
    for (j = 0; j < t2; j++)
	  bar(&x);
  return x;
}
