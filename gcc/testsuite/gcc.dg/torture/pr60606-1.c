/* { dg-do compile } */
/* { dg-options "-ffat-lto-objects" } */

int
f (void)
{
  register unsigned int r asm ("no-such-register"); /* { dg-error "invalid register name" } */
  return r;
}
