/* { dg-do compile } */
/* { dg-options " -fdump-rtl-all-slim -mavx -mvzeroupper -fexpensive-optimizations" } */

int
foo (void)
{
  return 0;
}

/* { dg-final { scan-rtl-dump-not "\\(insn " "vzeroupper" } }  */
