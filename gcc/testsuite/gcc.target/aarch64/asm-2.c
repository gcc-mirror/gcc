/* { dg-do compile } */
/* { dg-options "-O2" } */

int x;

void
f (void)
{
  asm volatile ("%a0" :: "X" (&x));
}
