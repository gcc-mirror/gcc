/* Test visibility attribute on definition of global variable that has
   already had a forward declaration. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*xyzzy" } } */

extern int xyzzy;

int 
__attribute__((visibility ("hidden"))) 
xyzzy = 5;
