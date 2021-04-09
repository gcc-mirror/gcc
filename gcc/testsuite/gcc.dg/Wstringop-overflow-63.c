/* PR middle-end/92936 - missing warning on a past-the-end store to a PHI
   Test case derived from gcc/opts-common.c.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

char* f (const void*, ...);

const char *
candidates_list_and_hint (const char *arg, char **str, const char *a[])
{
  size_t len = 0;
  int i;

  for (i = 0; a[i]; ++i)
    len += __builtin_strlen (a[i]) + 1;

  char *p = (char*)__builtin_malloc (len);
  *str = p;

  for (i = 0; a[i]; ++i)
    {
      len = __builtin_strlen (a[i]);
      __builtin_memcpy (p, a[i], len);
      p[len] = ' ';
      p += len + 1;
    }

  p[-1] = '\0';     // { dg-bogus "\\\[-Wstringop-overflow" }

  return f (arg, &a);
}
