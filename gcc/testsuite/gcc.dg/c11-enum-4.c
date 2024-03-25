/* Test C23 enumerations with fixed underlying type are diagnosed for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

enum e1 : int; /* { dg-error "ISO C does not support specifying 'enum' underlying types" } */
enum e2 : short { A }; /* { dg-error "ISO C does not support specifying 'enum' underlying types" } */
enum : short { B }; /* { dg-error "ISO C does not support specifying 'enum' underlying types" } */
