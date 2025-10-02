/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__((target_clones("dotprod", "sve+sve2")))
int foo ();

__attribute__((target_version("default")))
int foo ()
{
  return 0;
}

__attribute__((target_clones("sve", "sve2")))
int foo ()
{
  return 2;
}

int bar()
{
  return foo ();
}


/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._MsveMsve2:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Msve:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Msve2:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tbl\tfoo\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
// { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\.default\n" 1 } }
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\._Mdotprod\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\._MsveMsve2\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\._Msve\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\._Msve2\n" 1 } } */

