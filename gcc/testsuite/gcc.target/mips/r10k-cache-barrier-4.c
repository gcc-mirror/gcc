/* { dg-options "-O2 -mr10k-cache-barrier=store -mno-abicalls" } */

void bar (int *x);

/* Test that out-of-range stores to the frame are protected by cache
   barriers.  */

NOMIPS16 void
foo (int v)
{
  int x[8];
  bar (x);
  if (v & 1)
    x[0x100] = 0;
  if (v & 2)
    x[-0x100] = 0;
  bar (x);
}

/* { dg-final { scan-assembler-times "\tcache\t" 2 } } */
