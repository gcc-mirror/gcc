/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -fno-sanitize-recover=shift -std=c++11" } */

int
main ()
{
  int a = 1;
  a <<= (__SIZEOF_INT__ * __CHAR_BIT__ - 1);
  a = 16;
  a <<= (__SIZEOF_INT__ * __CHAR_BIT__ - 5);
}
