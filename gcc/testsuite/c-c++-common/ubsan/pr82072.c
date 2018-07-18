/* PR sanitizer/82072 */
/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

int
main ()
{
  long long l = -__LONG_LONG_MAX__ - 1;
  int i = 0;
  asm volatile ("" : "+r" (i));
  i -= l;
  asm volatile ("" : "+r" (i));
  i = -l;
  asm volatile ("" : "+r" (i));
  return 0;
}

/* { dg-output "signed integer overflow: 0 - -9223372036854775808 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -9223372036854775808 cannot be represented in type 'long long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself" } */
