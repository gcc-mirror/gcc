/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

void foo (void);

int test (long x, long n)
{
  n &= 0x3f;

  if (x & ((long)0x01 << n))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
