/* { dg-do compile } */
/* { dg-options "-O3 -march=core-avx2" } */
/* { dg-final { scan-assembler "vmovd\[\\t \]" } } */

int foo (void);
int a;
short *b;

void
bar (short x)
{
  while (a--)
    {
      int i, j = foo ();
      for (i = 0; i < j; ++i)
	*b++ = x;
    }
}
