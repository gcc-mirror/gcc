/* { dg-options "-O2 -mr10k-cache-barrier=store -mno-abicalls" } */

/* Test that loads are not unnecessarily protected.  */

int bar (int);

NOMIPS16 void
foo (int *ptr)
{
  *ptr = bar (*ptr);
}

/* { dg-final { scan-assembler-times "\tcache\t" 1 } } */
