/* PR sanitizer/115127 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

#include <stdio.h>

__attribute__((noipa)) int
f1 (unsigned a)
{
  return __builtin_clz (a) == 0;
}

__attribute__((noipa)) int
f2 (unsigned long a)
{
  return __builtin_clzl (a) != 0;
}

__attribute__((noipa)) int
f3 (unsigned long long a)
{
  return __builtin_clzll (a) == __CHAR_BIT__ * __SIZEOF_LONG_LONG__ - 1;
}

__attribute__((noipa)) int
f4 (unsigned a)
{
  return __builtin_clz (a) != __CHAR_BIT__ * __SIZEOF_INT__ - 1;
}

__attribute__((noipa)) int
f5 (unsigned long a)
{
  return __builtin_ctzl (a) == 0;
}

__attribute__((noipa)) int
f6 (unsigned long long a)
{
  return __builtin_ctzll (a) != 0;
}

__attribute__((noipa)) int
f7 (unsigned a)
{
  return __builtin_ctz (a) == 4;
}

__attribute__((noipa)) int
f8 (unsigned long a)
{
  return __builtin_ctzl (a) != 4;
}

__attribute__((noipa)) int
f9 (unsigned long long a)
{
  return __builtin_ctzll (a) >= 4;
}

__attribute__((noipa)) int
f10 (unsigned a)
{
  return __builtin_ctz (a) < 4;
}

int
main ()
{
  fprintf (stderr, "FOO MARKER1\n");
  f1 (0);
  f2 (0);
  f3 (0);
  f4 (0);
  fprintf (stderr, "FOO MARKER2\n");
  f5 (0);
  f6 (0);
  f7 (0);
  f8 (0);
  f9 (0);
  f10 (0);
  fprintf (stderr, "FOO MARKER3\n");
}

/* { dg-output "FOO MARKER1(\n|\r\n|\r)" } */
/* { dg-output "(\[^\n\r]*runtime error: passing zero to __builtin_clz\\\(\\\), which is not a valid argument\[^\n\r]*(\n|\r\n|\r)){4}" } */
/* { dg-output "FOO MARKER2(\n|\r\n|\r)" } */
/* { dg-output "(\[^\n\r]*runtime error: passing zero to __builtin_ctz\\\(\\\), which is not a valid argument\[^\n\r]*(\n|\r\n|\r)){6}" } */
/* { dg-output "FOO MARKER3" } */
