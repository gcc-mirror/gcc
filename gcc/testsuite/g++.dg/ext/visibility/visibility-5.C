/* Test visibility attribute on definition of a function that has
   already had a forward declaration. */
/* { dg-do compile { target *86-*-linux* } } */
/* { dg-final { scan-assembler "\\.hidden.*_Z3foov" } } */

void foo();

void 
 __attribute__((visibility ("hidden")))
foo() 
{ }
