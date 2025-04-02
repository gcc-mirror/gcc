/* { dg-do compile } */
/* { dg-options "-O0" } */

// FMV correctness with a default implementation and declarations of other
// versions

__attribute__ ((target_version ("default"))) int
foo ()
{
  return 1;
}

__attribute__ ((target_version ("dotprod"))) int
foo ();

__attribute__ ((target_version ("sve+sve2"))) int
foo ();

int
bar ()
{
  return foo ();
}

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._MsveMsve2:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tbl\tfoo\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
