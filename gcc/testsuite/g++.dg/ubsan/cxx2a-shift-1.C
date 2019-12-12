/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -fno-sanitize-recover=shift -std=c++2a" } */

int
main ()
{
  int a = 1;
  a <<= 31;
  a = 16;
  a <<= (__SIZEOF_INT__ * __CHAR_BIT__ - 5);
}
