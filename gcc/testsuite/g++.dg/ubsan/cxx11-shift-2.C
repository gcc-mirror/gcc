/* { dg-do run { target c++11 } } */
/* { dg-options "-fsanitize=shift -w" } */

int
main (void)
{
  int a = -42;
  a <<= 1;
}
/* { dg-output "left shift of negative value -42" } */
