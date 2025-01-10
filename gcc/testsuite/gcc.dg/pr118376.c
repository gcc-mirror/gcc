/* PR c/118376 */
/* { dg-do compile } */
/* { dg-options "-Wsign-conversion" } */

unsigned x;

void
foo ()
{
  __builtin_memset (&x, (long long) __builtin_stdc_rotate_right (x, 0), 1);
} /* { dg-warning "conversion to 'int' from 'long long int' may change the sign of the result" "" { target *-*-* } .-1 } */
