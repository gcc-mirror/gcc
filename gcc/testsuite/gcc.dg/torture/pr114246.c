/* { dg-do compile } */
/* { dg-additional-options "-w" } */

int a, b;

void
foo (void)
{
  __builtin_memcpy (&a, (char *)&b - 1, 2);
  __builtin_memcpy (&a, &b, 1);
}
