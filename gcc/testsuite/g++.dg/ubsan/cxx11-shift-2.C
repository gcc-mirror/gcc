/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -std=c++11" } */

int
main ()
{
  int a = -42;
  a <<= 1;
  a = -43;
  a <<= 0;
  a = -44;
  a <<= (__SIZEOF_INT__ * __CHAR_BIT__ - 1);
  a = 32;
  a <<= (__SIZEOF_INT__ * __CHAR_BIT__ - 3);
}
/* { dg-output "left shift of negative value -42.*" } */
/* { dg-output "left shift of negative value -43.*" } */
/* { dg-output "left shift of negative value -44.*" } */
/* { dg-output "left shift of 32 by \[0-9]* places cannot be represented in type 'int'" } */
