/* Test visibility attribute on forward declaration of global variable */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*xyzzy" } } */

extern int 
__attribute__((visibility ("hidden")))
xyzzy;

int xyzzy = 5;
