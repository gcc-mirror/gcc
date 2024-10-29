/* { dg-do run } */
/* { dg-options "-fsanitize=shift -std=c2y" } */

int
main ()
{
  int a = -42;
  unsigned b = 42;
  unsigned c = __builtin_stdc_rotate_left (b, a);
  unsigned d = __builtin_stdc_rotate_right (b, a - 1);
  volatile int e = c + d;
}
/* { dg-output "shift exponent -42 is negative\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent -43 is negative" } */
