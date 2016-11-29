/* { dg-do run } */
/* { dg-options "-fsanitize=shift-base -fno-sanitize=shift-exponent -w -std=c99" } */

int
main (void)
{
  int a = -42;
  int b = -43;
  volatile int c = 129;
  int d = 1;
  a << 1;
  b << c;
  a << 1;
  d <<= 31;
}
/* { dg-output "left shift of negative value -42\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*left shift of negative value -42\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*left shift of 1 by 31 places cannot be represented in type 'int'" } */
