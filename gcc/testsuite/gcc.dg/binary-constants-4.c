/* Test for binary integer constants: random errors.  */

/* Origin: Joerg Wunsch <j.gnu@uriah.heep.sax.de>.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
foo(void)
{
  double d;
  int i;

  d = 0b1101;
  d = 0b1101p1; /* { dg-error "invalid suffix \"p1\" on integer constant" } */
  d = 0x1101p1;
  i = 0b3011;   /* { dg-error "invalid suffix \"b3011\" on integer constant" } */
  i = 0b113;    /* { dg-error "invalid digit \"3\" in binary constant" } */
}
