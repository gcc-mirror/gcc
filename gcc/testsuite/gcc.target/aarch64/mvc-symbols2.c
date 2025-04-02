/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_clones ("default", "dotprod", "sve+sve2"))) int
foo ()
{
  return 1;
}

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._MsveMsve2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
