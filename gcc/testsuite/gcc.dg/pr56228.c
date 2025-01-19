/* PR target/56228 */
/* { dg-do assemble } */
/* { dg-options "-O2" } */

short a[14] = { 1, 2 };
short b[15] = { 3, 4 };

int
foo ()
{
  void (*fna) (void) = (void (*) (void)) a;
  void (*fnb) (void) = (void (*) (void)) b;
  fna (); /* { dg-warning "accessing program memory with data memory address.*" "" { target avr-*-* } } */
  fnb ();
  return a[1] == b[1];
}
