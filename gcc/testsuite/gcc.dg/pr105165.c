/* { dg-do compile } */
/* { dg-options "-O" } */

unsigned int _Complex a0;
unsigned int _Complex
foo (unsigned int _Complex a1, unsigned int _Complex a2)
{
  unsigned int _Complex x;
  asm goto ("" : "=r" (x) : : : lab); /* { dg-message "sorry, unimplemented" } */
  a0 = x;
 lab:
  return x + a1 + a2 + 1;
}
