/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -fno-sanitize-recover=shift -std=c++11" } */

int
main (void)
{
  int a = 1;
  a <<= 31;
  return 0;
}
