/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-ansi -pedantic" } */

int foo ()
{
  return 1;
}

register char *stack_ptr __asm ("%esp"); /* { dg-warning "file-scope declaration of 'stack_ptr' specifies 'register'" } */
