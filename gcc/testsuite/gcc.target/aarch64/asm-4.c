/* { dg-do compile } */
/* { dg-options "-O0" } */

int x;

void
f (void)
{
  asm volatile ("%a0" :: "X" (__builtin_extend_pointer (&x)));
}
