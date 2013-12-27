/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w" } */

int
main (void)
{
  int a = 1;
  volatile int b = -5;
  long long int c = -6;

  a << -3;
  1 << -4;
  1 << b;
  a << c;
  a << (b + c);

  return 0;
}
/* { dg-output "shift exponent -3 is negative(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent -4 is negative(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent -5 is negative(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent -6 is negative(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent -11 is negative" } */
