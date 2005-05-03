/* { dg-do compile } */
/* { dg-options "-m32" } */

register unsigned int EAX asm ("r14"); /* { dg-error "register name" } */

void foo ()
{
  EAX = 0;
}
