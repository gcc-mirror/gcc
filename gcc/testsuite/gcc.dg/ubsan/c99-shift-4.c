/* { dg-do run } */
/* { dg-options "-fsanitize=shift-exponent -fno-sanitize=shift-base -w -std=c99" } */

int
main (void)
{
  int a = -42;
  int b = -43;
  volatile int c = 129;
  int d = 1;
  b << c;
  a << 1;
  a << 1;
  d <<= 31;
  b << (c + 1);
}
/* { dg-output "shift exponent 129 is too large for \[^\n\r]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 130 is too large for \[^\n\r]*-bit type 'int'" } */
