/* Test visibility attribute on definition of a function that has
   already had a forward declaration. */
/* { dg-require-visibility "" }
/* { dg-final { scan-hidden "_Z3foov" } } */

void foo();

void 
 __attribute__((visibility ("hidden")))
foo() 
{ }
