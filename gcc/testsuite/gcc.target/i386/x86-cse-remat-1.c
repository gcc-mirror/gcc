/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */

/* The zero vector stored in the loop is live across the calls.  It
   should be rematerialized, not spilled, so the stack doesn't need to
   be realigned.  */

extern void bar (void);
extern int cond (int);

void
foo (int *__restrict a, int *__restrict b, int *__restrict c, int n)
{
  for (int i = 0; i < 8; i++)
    a[i] = 0;
  for (int j = 0; j < n; j++)
    {
      if (cond (j))
	bar ();
      for (int i = 0; i < 8; i++)
	b[i + 8 * j] = 0;
    }
  for (int i = 0; i < 8; i++)
    c[i] = 0;
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]+\\\$-32, %\[re\]sp" } } */
/* { dg-final { scan-assembler-not "vmovdq\[au\]\[ \t\]+%ymm\[0-9\]+, -?\[0-9\]*\\(%\[re\]\[sb\]p\\)" } } */
