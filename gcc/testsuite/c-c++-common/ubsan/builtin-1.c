/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

#include <stdio.h>

__attribute__((noinline, noclone)) unsigned long long
foo (unsigned int x, unsigned long int y, unsigned long long int z, __UINTMAX_TYPE__ w)
{
  unsigned long long ret = 0;
  fprintf (stderr, "FOO MARKER1\n");
  ret += __builtin_ctz (x);
  ret += __builtin_ctzl (y);
  ret += __builtin_ctzll (z);
  ret += __builtin_ctzimax (w);
  fprintf (stderr, "FOO MARKER2\n");
  ret += __builtin_clz (x);
  ret += __builtin_clzl (y);
  ret += __builtin_clzll (z);
  ret += __builtin_clzimax (w);
  fprintf (stderr, "FOO MARKER3\n");
  return ret;
}

int
main ()
{
  volatile __UINTMAX_TYPE__ t = 0;
  t = foo (t, t, t, t);
  return 0;
}

/* { dg-output "FOO MARKER1(\n|\r\n|\r)" } */
/* { dg-output "(\[^\n\r]*runtime error: passing zero to __builtin_ctz\\\(\\\), which is not a valid argument\[^\n\r]*(\n|\r\n|\r)){4}" } */
/* { dg-output "FOO MARKER2(\n|\r\n|\r)" } */
/* { dg-output "(\[^\n\r]*runtime error: passing zero to __builtin_clz\\\(\\\), which is not a valid argument\[^\n\r]*(\n|\r\n|\r)){4}" } */
/* { dg-output "FOO MARKER3" } */
