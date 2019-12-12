/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "" } */

register unsigned int EAX asm ("r14"); /* { dg-error "cannot be accessed" } */

void foo ()
{
  EAX = 0;
}
