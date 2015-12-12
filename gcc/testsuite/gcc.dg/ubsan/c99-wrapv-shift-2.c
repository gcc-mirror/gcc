/* { dg-do run } */
/* { dg-options "-fsanitize=shift -fwrapv -w -std=c99" } */

int
main (void)
{
  int a = 1;
  a <<= 31;
}
