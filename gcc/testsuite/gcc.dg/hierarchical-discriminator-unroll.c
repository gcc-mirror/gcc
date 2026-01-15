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

/* Expected discriminators from the assembly (hierarchical format: [Base:8][Multiplicity:7][CopyID:11][Unused:6]):
   Loop unrolling with ndupl=4:
   - allocate_copyid_base(loc, 4) returns base=1 (first time)
   - Iteration 0: copyid = 1+0 = 1, multiplicity=0 → 0|(0<<8)|(1<<15) = 32768
   - Iteration 1: copyid = 1+1 = 2, multiplicity=0 → 0|(0<<8)|(2<<15) = 65536
   - Iteration 2: copyid = 1+2 = 3, multiplicity=0 → 0|(0<<8)|(3<<15) = 98304
   - Iteration 3: copyid = 1+3 = 4, multiplicity=0 → 0|(0<<8)|(4<<15) = 131072
*/

/* Each unrolled iteration should have a different discriminator */
/* { dg-final { scan-assembler "\\.loc 1 17 7 is_stmt 0 discriminator 32768" } } */
/* { dg-final { scan-assembler "\\.loc 1 17 7 is_stmt 0 discriminator 65536" } } */
/* { dg-final { scan-assembler "\\.loc 1 17 7 is_stmt 0 discriminator 98304" } } */
/* { dg-final { scan-assembler "\\.loc 1 17 7 is_stmt 0 discriminator 131072" } } */
