/* Test that visibility attribute on declaration extends to definition. */
/* { dg-do compile { target *86-*-linux* } } */
/* { dg-final { scan-assembler "\\.hidden.*_Z3foov" } } */

void __attribute__((visibility ("hidden"))) foo();

void foo() { }
