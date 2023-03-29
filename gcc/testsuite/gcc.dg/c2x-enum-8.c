/* Test C2x enumerations with fixed underlying type.  Test -Wc11-c2x-compat
   warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

enum e1 : int; /* { dg-warning "ISO C does not support specifying 'enum' underlying types before" } */
enum e2 : short { E2 }; /* { dg-warning "ISO C does not support specifying 'enum' underlying types before" } */
