/* { dg-do compile } */
/* { dg-options "-O2" } */

struct B { unsigned bit0 : 1; unsigned bit1 : 1; };

void
bar  (struct B *b, int x)
{
  b->bit0 |= x;
}

/* This fails to combine in 32bit mode but not for x32.  */
/* { dg-final { scan-assembler-times {\tand[lq]} 1 { xfail { { ! x32 } && ilp32 } } } } */
/* { dg-final { scan-assembler-times {\tor} 1 { xfail { { ! x32 } && ilp32 } } } } */
