/* { dg-additional-options "-fdump-analyzer" } */

void test (void)
{
}

/* Verify that we log the named constants that we look up in the TU
   (before the analysis pass runs).
     { dg-final { scan-file fdump-analyzer-1.c.analyzer.txt "maybe_stash_named_constant: name: 'O_ACCMODE'" } }

   Verify that we log the main part of the analysis (part of the
   analysis pass):
     { dg-final { scan-file fdump-analyzer-1.c.analyzer.txt "ana::run_checkers" } }
 */
