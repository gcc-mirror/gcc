/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

void foo (void);

int test (int x, int n)
{
  n &= 0x1f;

  if (x & (0x01 << n))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
