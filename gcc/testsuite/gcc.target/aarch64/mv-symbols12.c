/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo () { return 1; }

__attribute__ ((target_version ("dotprod"))) int
foo () { return 3; }

int bar ()
{
  int (*test)() = foo;

  test();
}

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 1 } } */

/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\], foo\._Mdotprod\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\], foo\.default\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 1 } } */
