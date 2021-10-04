/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */

int
main ()
{
  register long *sp __asm__("sp");
  return 0;
}
