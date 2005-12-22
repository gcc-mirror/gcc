/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

int foo ()
{
  return 1;
}

register char *stack_ptr __asm ("%esp");
