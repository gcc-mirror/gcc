/* { dg-do compile } */
/* { dg-options "-O0" } */

// FMV correctness with default declaration, and implementations of other
// versions.

__attribute__ ((target_version ("default"))) int
foo ();

__attribute__ ((target_version ("dotprod"))) int
foo ()
{
  return 3;
}
__attribute__ ((target_version ("sve+sve2"))) int
foo ()
{
  return 5;
}

int
bar ()
{
  return foo ();
}

/* When updating any of the symbol names in these tests, make sure to also
   update any tests for their absence in mvc-symbolsN.C */

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._MsveMsve2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tbl\tfoo\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 0 } } */
