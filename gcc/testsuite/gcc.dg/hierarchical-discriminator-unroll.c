/* Test that loop unrolling assigns copyid discriminators.
   { dg-do compile }
   { dg-options "-S -O2 -g  -fno-tree-vectorize" } */

int a[100];
int
test_unroll (void)
{
  int sum = 0;
  int i;
  
  /* Small fixed-count loop that should be completely unrolled */
  #pragma GCC unroll 4
  for (i = 0; i < 4; i++)
    {
      /* Each unrolled iteration gets a distinct copyid (1, 2, 3, 4) */
      asm ("nop");
      sum += a[i] * 2; 
    }
  
  return sum;
}

/* Loop unrolling with #pragma GCC unroll 4 should create 4 copies with distinct
   copyids in the hierarchical discriminator format: [Base:8][Multiplicity:7][CopyID:11][Unused:6].
   Each unrolled iteration should get a different copyid, resulting in different discriminators.
   The exact values depend on what other passes have run, but all should be non-zero. */

/* Check that unrolled iterations have non-zero discriminators on the asm statement line. */
/* { dg-final { scan-assembler "\\.loc 1 17 7 is_stmt 0 discriminator (\[1-9\]\[0-9\]*|0x\[1-9a-fA-F\]\[0-9a-fA-F\]*)" } } */
