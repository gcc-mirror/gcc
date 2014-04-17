/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */

#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)
#define LONG_MAX __LONG_MAX__
#define LONG_MIN (-__LONG_MAX__ - 1L)
#define LLONG_MAX __LONG_LONG_MAX__
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1L)

int
main (void)
{
  volatile int j = INT_MIN;
  volatile int i = 1;
  volatile int k = j - i;
  j--;
  j = INT_MIN + 100;
  j -= (1 << 10);

  j = INT_MIN;
  i = -1;
  k = j - -i;

  i = INT_MIN + 1000;
  i -= (1 << 20);

  volatile long int l = LONG_MIN;
  volatile long int m = 1;
  volatile long int n = l - m;
  l--;
  l = LONG_MIN + 100;
  l -= (1 << 10);

  l = LONG_MIN;
  m = -1;
  n = l - -m;

  m = LONG_MIN + 1000;
  m -= (1 << 20);

  return 0;
}

/* { dg-output "signed integer overflow: -2147483648 - 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 \\+ -1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483548 \\+ -1024 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 \\+ -1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147482648 \\+ -1048576 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* - 1 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* \\+ -1 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* \\+ -1024 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* \\+ -1 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* \\+ -1048576 cannot be represented in type 'long int'\[^\n\r]*" } */
