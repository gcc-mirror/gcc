/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

long long int __attribute__ ((noinline, noclone))
foo (long long int i, long long int j)
{
  asm ("");
  return i + j;
}

int
main (void)
{
  foo (2LL, __LONG_LONG_MAX__);
  return 0;
}

/* { dg-output "signed integer overflow: 2 \\+ 9223372036854775807 cannot be represented in type 'long long int'" } */
