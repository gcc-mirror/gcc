/* Test for octal integer constants: random errors.  */

/* Origin: Joerg Wunsch <j.gnu@uriah.heep.sax.de>.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void
foo(void)
{
  double d;
  int i;

  d = 0o1307;
  d = 0o1307p1; /* { dg-error "invalid suffix 'p1' on integer constant" } */
  d = 0x1307p1;
  i = 0oa011;   /* { dg-error "invalid suffix 'oa011' on integer constant" } */
  i = 0o118;    /* { dg-error "invalid digit '8' in octal constant" } */
}
