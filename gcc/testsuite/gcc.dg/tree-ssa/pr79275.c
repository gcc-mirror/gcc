/* PR middle-end/79275 - -Wformat-overflow false positive exceeding INT_MAX
   in glibc sysdeps/posix/tempname.c
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow=1 -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

void f (char *dst, size_t n, const char *s)
{
  if (n < 2 || __INT_MAX__ - 2 < n)
    n = 2;

  __builtin_sprintf (dst, "%.*s %.*s", (int)n, s, (int)n, s);   /* { dg-bogus "INT_MAX" } */
}
