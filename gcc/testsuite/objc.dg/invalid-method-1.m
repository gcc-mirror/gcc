/* { dg-do compile } */
/* Test that we keep compiling if a method definition is found outside
   of an @implementation context.
*/

+ (void)C { } /* { dg-error "method definition not in @implementation context" } */

/* We have another error here to test that the compiler is still going and
   finding errors in the rest of the code.
*/
@compatibility_alias class1 class2; /* { dg-warning "annot find class" } */
