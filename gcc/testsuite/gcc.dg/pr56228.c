/* PR target/56228 */
/* { dg-do assemble } */
/* { dg-options "-O2" } */
/* { dg-skip-if "accessing program memory with data memory address" { "avr-*-*" } { "*" } { "" } } */

short a[14] = { 1, 2 };
short b[15] = { 3, 4 };

int
foo ()
{
  void (*fna) (void) = (void (*) (void)) a;
  void (*fnb) (void) = (void (*) (void)) b;
  fna ();
  fnb ();
  return a[1] == b[1];
}
