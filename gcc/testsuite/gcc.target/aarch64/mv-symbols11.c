/* { dg-do compile } */
/* { dg-options "-O0" } */

// Check that types can be combined

__attribute__ ((target_version ("default"))) int
foo (int a, int (*b)[4]) { return 1; }

__attribute__ ((target_version ("dotprod"))) int
foo (int a, int (*b)[]) { return 3; }

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
