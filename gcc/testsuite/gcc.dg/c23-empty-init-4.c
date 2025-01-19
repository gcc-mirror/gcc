/* Test C23 support for empty initializers: invalid for empty arrays in
   compound literals (bug 114266).  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

void
f ()
{
  (int []) { }; /* { dg-error "array of unknown size with empty initializer" } */
}
