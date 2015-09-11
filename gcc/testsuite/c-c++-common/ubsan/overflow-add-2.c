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
  volatile int j = INT_MAX;
  volatile int i = 1;
  volatile int k = j + i;
  k = i + j;
  j++;
  j = INT_MAX - 100;
  j += (1 << 10);

  j = INT_MIN;
  i = -1;
  k = i + j;
  k = j + i;
  j = INT_MIN + 100;
  j += -(1 << 10);

  volatile long int m = LONG_MAX;
  volatile long int n = 1;
  volatile long int o = m + n;
  o = n + m;
  m++;
  m = LONG_MAX - 100;
  m += (1 << 10);

  m = LONG_MIN;
  n = -1;
  o = m + n;
  o = n + m;
  m = LONG_MIN + 100;
  m += -(1 << 10);

  return 0;
}

/* { dg-output "signed integer overflow: 2147483647 \\+ 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 1 \\+ 2147483647 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 2147483647 \\+ 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 2147483547 \\+ 1024 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -1 \\+ -2147483648 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 \\+ -1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483548 \\+ -1024 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: \[^\n\r]* \\+ 1 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 1 \\+ \[^\n\r]* cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: \[^\n\r]* \\+ 1 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: \[^\n\r]* \\+ 1024 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* \\+ -1 cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -1 \\+ -\[^\n\r]* cannot be represented in type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -\[^\n\r]* \\+ -1024 cannot be represented in type 'long int'" } */
