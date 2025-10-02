/* { dg-do compile } */
/* { dg-options "-O0" } */

// FMV correctness with declarations but no implementation

__attribute__ ((target_version ("default"))) int
foo ();

__attribute__ ((target_version ("dotprod"))) int
foo ();

__attribute__ ((target_version ("sve+sve2"))) int
foo ();

int
bar ()
{
  return foo ();
}

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._MsveMsve2:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tbl\tfoo\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 0 } } */
