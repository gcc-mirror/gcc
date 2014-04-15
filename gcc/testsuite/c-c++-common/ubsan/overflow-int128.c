/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

/* 2^127 - 1 */
#define INT128_MAX (__int128) (((unsigned __int128) 1 << ((__SIZEOF_INT128__ * __CHAR_BIT__) - 1)) - 1)
#define INT128_MIN (-INT128_MAX - 1)

int
main (void)
{
  volatile __int128 i = INT128_MAX;
  volatile __int128 j = 1;
  volatile __int128 k = i + j;
  k = j + i;
  i++;
  j = INT128_MAX - 100;
  j += (1 << 10);

  j = INT128_MIN;
  i = -1;
  k = i + j;
  k = j + i;
  j--;
  j = INT128_MIN + 100;
  j += -(1 << 10);

  i = INT128_MAX;
  j = 2;
  k = i * j;

  i = INT128_MIN;
  i = -i;

  return 0;
}

/* { dg-output "signed integer overflow: 0x7fffffffffffffffffffffffffffffff \\+ 1 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 1 \\+ 0x7fffffffffffffffffffffffffffffff cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x7fffffffffffffffffffffffffffffff \\+ 1 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x7fffffffffffffffffffffffffffff9b \\+ 1024 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -1 \\+ 0x80000000000000000000000000000000 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x80000000000000000000000000000000 \\+ -1 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x80000000000000000000000000000000 \\+ -1 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x80000000000000000000000000000064 \\+ -1024 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 0x7fffffffffffffffffffffffffffffff \\* 2 cannot be represented in type '__int128'(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of 0x80000000000000000000000000000000 cannot be represented in type '__int128'; cast to an unsigned type to negate this value to itself\[^\n\r]*(\n|\r\n|\r)" } */
