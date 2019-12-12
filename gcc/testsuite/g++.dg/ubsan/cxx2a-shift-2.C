/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -fno-sanitize-recover=shift -std=c++2a" } */

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
