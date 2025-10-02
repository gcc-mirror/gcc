/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo ();

int bar ()
{
  int (*test)() = foo;

  test();
}

__attribute__ ((target_version ("dotprod"))) int
foo () { return 3; }

/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\._Mdotprod:\n" 1 } } */

/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\], foo\._Mdotprod\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\], foo\.default\n" 0 } } */

/* { dg-final { scan-assembler-times "\n\tadrp\tx\[0-9\]+, foo\n" 1 } } */

/* { dg-final { scan-assembler-times "\n\t\.type\tfoo, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\tfoo,foo\.resolver\n" 0 } } */

