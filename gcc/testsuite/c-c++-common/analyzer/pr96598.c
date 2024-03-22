/* { dg-additional-options "-O0 -fsanitize=undefined" } */

extern char *foo (char *dest, const char *src)
  __attribute__ ((__nonnull__ (1, 2)));

unsigned bar(const char *str)
  __attribute__ ((__nonnull__ ()));

unsigned test(const char *str, unsigned **pv)
  __attribute__ ((__nonnull__ ()));

unsigned test(const char* str, unsigned **pv)
{
  char buffer[130];

  *pv = 0;

  foo(buffer, str);
  if (bar(buffer))
    {
      const char *ptr = 0;
      foo(buffer, str);
      return bar(buffer);
    }
  return 0;
}
