/* Test visibility attribute on function definition. */
/* { dg-do compile { target *86-*-linux* } } */
/* { dg-final { scan-assembler "\\.hidden.*_Z3foov" } } */

void
__attribute__((visibility ("hidden")))
foo()
{ }
