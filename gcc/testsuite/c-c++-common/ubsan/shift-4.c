/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w" } */

struct S { unsigned long long int b:40; } s;

int
main ()
{
  s.b = 2;
  s.b <<= 120;
  return 0;
}

/* { dg-output "shift exponent 120 is too large" } */
