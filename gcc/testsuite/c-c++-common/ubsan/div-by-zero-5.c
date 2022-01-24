/* { dg-do compile } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

void
foo (void)
{
  int A[-2 / -1] = {};
}
