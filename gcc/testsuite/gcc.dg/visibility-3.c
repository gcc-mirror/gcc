/* Test visibility attribute on forward declaration of global variable */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*xyzzy" } } */

int
__attribute__((visibility ("hidden")))
xyzzy = 5;
