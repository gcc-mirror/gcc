/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -Wno-div-by-zero" } */

int
main (void)
{
  volatile int a = 0;
  volatile long long int b = 0;
  volatile unsigned int c = 1;

  a / b;
  0 / 0;
  a / 0;
  0 / b;
  2 / --c;

  return 0;
}

/* { dg-output "division by zero(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero" } */
