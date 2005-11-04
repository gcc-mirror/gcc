/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-ansi -pedantic" } */

int foo ()
{
  return 1;
}

register char *stack_ptr __asm ("%esp"); /* { dg-warning "warning: file-scope declaration of 'stack_ptr' specifies 'register'" } */
