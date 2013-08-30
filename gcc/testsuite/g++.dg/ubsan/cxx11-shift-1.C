/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -std=c++11" } */

int
main (void)
{
  int a = 1;
  a <<= 31;
}
