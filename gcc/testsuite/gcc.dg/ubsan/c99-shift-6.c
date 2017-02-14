/* { dg-do run } */
/* { dg-shouldfail "ubsan" } */
/* { dg-options "-fsanitize=shift -fsanitize-recover=shift-exponent -fno-sanitize-recover=shift-base -w -std=c99" } */

int
main (void)
{
  int a = -42;
  int b = -43;
  volatile int c = 129;
  1 << c;
  1 << (c + 1);
  a << 1;
  b << 1;
}
/* { dg-output "shift exponent 129 is too large for \[^\n\r]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 130 is too large for \[^\n\r]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*left shift of negative value -42\[^\n\r]*(\n|\r\n|\r)" } */
