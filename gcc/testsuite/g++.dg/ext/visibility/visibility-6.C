/* Test visibility attribute on definition of global variable that has
   already had a forward declaration. */
/* { dg-do compile { target *86-*-linux* } } */
/* { dg-final { scan-assembler "\\.hidden.*xyzzy" } } */

extern int xyzzy;

int 
__attribute__((visibility ("hidden")))
xyzzy = 5;
