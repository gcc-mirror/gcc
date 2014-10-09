/* { dg-do compile } */
/* { dg-options "-O" } */

int
f (void)
{
  register unsigned int r asm ("cc"); /* { dg-error "not general enough|suitable for data type" } */
  return r;
}
