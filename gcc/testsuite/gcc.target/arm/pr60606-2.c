/* { dg-do compile } */
/* { dg-options "-O" } */

int
f (void)
{
  register unsigned pc asm ("pc"); /* { dg-error "not general enough" } */
  
  return pc > 0x12345678;
}
