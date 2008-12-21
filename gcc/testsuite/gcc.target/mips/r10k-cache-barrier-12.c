/* { dg-options "-O2 -mr10k-cache-barrier=load-store -mno-abicalls" } */

/* Test that loads are correctly protected.  */

int bar (int);

NOMIPS16 void
foo (int *ptr)
{
  *ptr = bar (*ptr);
}

/* { dg-final { scan-assembler-times "\tcache\t" 2 } } */
