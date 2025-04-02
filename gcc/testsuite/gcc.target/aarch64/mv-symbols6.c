/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) int
foo ()
{
  return 1;
}

int bar()
{
  return foo();
}

/* { dg-final { scan-assembler-times "\nfoo:\n" 0 } } */
/* { dg-final { scan-assembler-times "\nfoo\.default:\n" 1 } } */
/* { dg-final { scan-assembler-times "\nfoo\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "bl\tfoo.default\n" 1 } } */
/* { dg-final { scan-assembler-times ".global\tfoo\n" 1 } } */
/* { dg-final { scan-assembler-times ".set\tfoo,foo.default\n" 1 } } */
