/* { dg-do run } */
/* { dg-shouldfail "ubsan" } */
/* { dg-options "-fsanitize=float-divide-by-zero -fno-sanitize-recover=float-divide-by-zero -fsanitize-recover=integer-divide-by-zero" } */

int
main (void)
{
  volatile float a = 1.3f;
  volatile double b = 0.0;
  volatile int c = 4;
  volatile float res;

  res = a / b;

  return 0;
}

/* { dg-output "division by zero" } */
