/* { dg-do compile } */
/* { dg-options "-O" } */

int
f (void)
{
  register unsigned int r[50] asm ("r1"); /* { dg-error "suitable for a register" } */
  return r[1];
}
