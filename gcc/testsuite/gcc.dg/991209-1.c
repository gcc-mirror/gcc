/* { dg-do compile { target i?86-*-* } } */

int foo ()
{
  return 1;
}

register char *stack_ptr __asm ("%esp");
