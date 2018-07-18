/* { dg-do compile } */
/* { dg-options "-O2" } */

int x;

void
f (void)
{
  asm volatile ("%y0" :: "X" (x)); /* { dg-error "invalid" } */
}
