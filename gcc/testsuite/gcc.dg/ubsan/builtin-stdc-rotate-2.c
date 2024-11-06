/* { dg-do run } */
/* { dg-options "-fsanitize=shift -fno-sanitize-recover=shift -std=c2y" } */

int
main ()
{
  int a = sizeof (unsigned) * __CHAR_BIT__ + 1;
  unsigned b = 42;
  unsigned c = __builtin_stdc_rotate_left (b, a);
  unsigned d = __builtin_stdc_rotate_right (b, a);
  volatile int e = c + d;
  if (c != 84 || d != 21)
    __builtin_abort ();
}
