/* { dg-do compile } */
/* { dg-options "-O2 -mno-accumulate-outgoing-args -mincoming-stack-boundary=4 -mpreferred-stack-boundary=6" } */

extern void foo (int, int, int, int, int, int, int);

void
bar (void)
{
  foo (1, 2, 3, 4, 5, 6, 7);
}

/* { dg-final { scan-assembler "lea\[lq\]?\[\\t \]*\[0-9\]*\\(%\[er\]sp\\)" } } */
