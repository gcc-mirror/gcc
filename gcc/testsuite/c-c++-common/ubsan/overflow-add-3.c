/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable -fno-sanitize-recover=signed-integer-overflow" } */
/* { dg-shouldfail "ubsan" } */

#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)

int
main (void)
{
  volatile int j = INT_MAX;
  volatile int i = 1;
  volatile int k = j + i;
  return 0;
}

/* { dg-output "signed integer overflow: 2147483647 \\+ 1 cannot be represented in type 'int'" } */
