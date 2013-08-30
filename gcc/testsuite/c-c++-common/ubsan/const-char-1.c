/* { dg-do compile } */
/* { dg-options "-fsanitize=shift" } */

void
foo (void)
{
  int y = 1 << 2;
  __builtin_printf ("%d\n", y);
}
