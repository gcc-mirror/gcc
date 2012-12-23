/* { dg-options "-mr10k-cache-barrier=store -mips4 -mbranch-likely -mno-abicalls" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

unsigned char *bar (int);

/* Test that code after a branch-likely does not get an unnecessary
   cache barrier.  */

NOMIPS16 void
foo (unsigned char *n)
{
  /* n starts in $4, but will be in $2 after the call to bar.
     Encourage it to be in $2 on entry to the loop as well,
     by doing some computation on it beforehand (D?ADDIU $2,$4,4).
     dbr_schedule should then pull the *n load (L[WD] ...,0($2))
     into the delay slot.  */
  n += 4;
  do
    n = bar (*n + 1);
  while (n);
  /* The preceding branch should be a branch likely, with the shift as
     its delay slot.  We therefore don't need a cache barrier here.  */
  n[0] = 0;
}

/* { dg-final { scan-assembler-not "\tcache\t" } } */
