/* { dg-do run { target int128 } } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#define INT_MIN (-__INT_MAX__ - 1)

int
main (void)
{
  int j = INT_MIN;
  return -j;
}

/* { dg-output "negation of -2147483648 cannot be represented in type 'int'; cast to an unsigned type to negate this value to itself(\n|\r\n|\r)" } */
