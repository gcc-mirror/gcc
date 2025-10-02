/* { dg-do compile } */
/* { dg-options "-O0" } */

int foo ();

__attribute__ ((target_version ("default")))  int
foo ()
{
  return 1;
}

__attribute__ ((target_version ("dotprod"))) int
foo ()
{
  return 3;
}

int
bar ()
{
  return foo ();
}

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 1 } } */

/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx., foo\._Mdotprod\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx., foo\.default\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\tbl\tfoo\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
