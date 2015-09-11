/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */

#define INT_MIN (-__INT_MAX__ - 1)
#define LONG_MIN (-__LONG_MAX__ - 1L)
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1LL)

int
main (void)
{
  int e = 1, f = -1;
  volatile int i = INT_MIN;
  volatile int i2 = i & (((((((-i) + 1) - 1) + 1) - 1) + 1) - 1);
  i2 = -(i + e + f);
  i = -i;

  volatile long int li = LONG_MIN;
  volatile long int li2 = li & (((((((-li) + 1) - 1) + 1) - 1) + 1) - 1);
  li2 = -(li + e + f);
  li = -li;

  volatile long long int lli = LLONG_MIN;
  volatile long long int lli2 = lli & (((((((-lli) + 1) - 1) + 1) - 1) + 1) - 1);
  lli2 = -(lli + e + f);
  lli = -lli;

  return 0;
}

/* { dg-output "negation of -2147483648 cannot be represented in type 'int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -2147483648 cannot be represented in type 'int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -2147483648 cannot be represented in type 'int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -\[^\n\r]* cannot be represented in type 'long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -\[^\n\r]* cannot be represented in type 'long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -\[^\n\r]* cannot be represented in type 'long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -9223372036854775808 cannot be represented in type 'long long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -9223372036854775808 cannot be represented in type 'long long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -9223372036854775808 cannot be represented in type 'long long int'\[^\n\r]*; cast to an unsigned type to negate this value to itself" } */
