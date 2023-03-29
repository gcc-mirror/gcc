/* Test C2x enumerations with fixed underlying type are diagnosed for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

enum e1 : int; /* { dg-warning "ISO C does not support specifying 'enum' underlying types" } */
enum e2 : short { A }; /* { dg-warning "ISO C does not support specifying 'enum' underlying types" } */
enum : short { B }; /* { dg-warning "ISO C does not support specifying 'enum' underlying types" } */
