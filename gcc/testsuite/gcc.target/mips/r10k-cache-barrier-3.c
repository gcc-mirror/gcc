/* { dg-options "-mr10k-cache-barrier=store -mno-abicalls" } */

/* Test that in-range stores to the frame are not protected by
   cache barriers.  */

void bar (int *x);

NOMIPS16 void
foo (int v)
{
  int x[0x100000];
  bar (x);
  x[0x20] = v;
  bar (x);
}

/* { dg-final { scan-assembler-not "\tcache\t" } } */
