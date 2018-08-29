/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-accumulate-outgoing-args -mincoming-stack-boundary=4 -mpreferred-stack-boundary=6" } */

extern void foo (int, int, int) __attribute__ ((regparm(3)));

void
bar (int i1, int i2, int i3, int i4)
{
  foo (i1, i2, i3);
}

/* { dg-final { scan-assembler-not "lea\[l\]?\[\\t \]*\[0-9\]*\\(%esp\\)" } } */
