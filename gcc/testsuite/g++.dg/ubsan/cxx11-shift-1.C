/* { dg-do run { target c++11 } } */
/* { dg-options "-fsanitize=shift -w" } */

int
main (void)
{
  int a = 1;
  a <<= 31;
}
