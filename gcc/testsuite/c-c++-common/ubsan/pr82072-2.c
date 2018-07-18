/* PR sanitizer/82072 */
/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

int
main ()
{
  long long int l = -__LONG_LONG_MAX__ - 1;
  unsigned int u;
  u = -l;
  asm volatile ("" : "+r" (u));
  return 0;
}

/* { dg-output "negation of -9223372036854775808 cannot be represented in type 'long long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself" } */
