/* Test visibility attribute on definition of a function that has
   already had a forward declaration. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*foo" } } */

void foo();

void
 __attribute__((visibility ("hidden")))
foo()
{ }
