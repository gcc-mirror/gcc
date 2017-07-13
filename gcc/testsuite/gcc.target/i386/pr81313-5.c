/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-accumulate-outgoing-args -mincoming-stack-boundary=4 -mpreferred-stack-boundary=6" } */

extern void foo (int, int, int, int, int, int);

void
bar (int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
  foo (i1, i2, i3, i4, i5, i6);
}

/* { dg-final { scan-assembler-not "lea\[lq\]?\[\\t \]*\[0-9\]*\\(%\[er\]sp\\)" } } */
