/* Test that visibility attribute on declaration extends to definition. */
/* { dg-require-visibility "" }
/* { dg-final { scan-hidden "_Z3foov" } } */

void __attribute__((visibility ("hidden"))) foo();

void foo() { }
