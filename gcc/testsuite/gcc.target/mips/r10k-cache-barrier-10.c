/* { dg-options "-mr10k-cache-barrier=store -mips4 -mbranch-likely -mno-abicalls" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

unsigned char *bar (int);

/* Test that code after a branch-likely does not get an unnecessary
   cache barrier.  */

NOMIPS16 void
foo (unsigned char *n)
{
  do
    n = bar (*n + 1);
  while (n);
  /* The preceding branch should be a branch likely, with the shift as
     its delay slot.  We therefore don't need a cache barrier here.  */
  n[0] = 0;
}

/* { dg-final { scan-assembler-not "\tcache\t" } } */
