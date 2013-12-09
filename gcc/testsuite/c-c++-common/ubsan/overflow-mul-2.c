/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#define INT_MAX __INT_MAX__
#define LONG_MAX __LONG_MAX__

int
main (void)
{
  volatile int j = INT_MAX;
  volatile int i = 2;
  volatile int k = j * i;
  k = i * j;

  volatile long int m = LONG_MAX;
  volatile long int n = 2;
  volatile long int o = m * n;
  o = n * m;

  return 0;
}

/* { dg-output "signed integer overflow: 2147483647 \\* 2 cannot be represented in type 'int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 2 \\* 2147483647 cannot be represented in type 'int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: \[^\n\r]* \\* 2 cannot be represented in type 'long int'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 2 \\* \[^\n\r]* cannot be represented in type 'long int'(\n|\r\n|\r)" } */
