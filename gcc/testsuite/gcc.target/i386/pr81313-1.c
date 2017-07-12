/* { dg-do compile } */
/* { dg-options "-O2 -mno-accumulate-outgoing-args -mincoming-stack-boundary=4 -mpreferred-stack-boundary=6" } */

extern void foo (void);

void
bar (void)
{
  foo ();
}

/* { dg-final { scan-assembler-not "lea\[lq\]?\[\\t \]*\[0-9\]*\\(%\[er\]sp\\)" } } */
