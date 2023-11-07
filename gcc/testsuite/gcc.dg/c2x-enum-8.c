/* Test C23 enumerations with fixed underlying type.  Test -Wc11-c23-compat
   warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

enum e1 : int; /* { dg-warning "ISO C does not support specifying 'enum' underlying types before" } */
enum e2 : short { E2 }; /* { dg-warning "ISO C does not support specifying 'enum' underlying types before" } */
