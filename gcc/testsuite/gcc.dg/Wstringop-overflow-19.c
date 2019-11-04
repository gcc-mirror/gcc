/* PR middle-end/92014 - bogus warning: writing 8 bytes into a region
   of size 1 in timezone/zic.c
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct
{
  char *s1, *s2;
  char c;
} z;


void f (char **a, int i, int j)
{
  char * cp = __builtin_strchr (a[i], '%');

  if (cp && *++cp != 's')
    return;

  z.s1 = __builtin_strdup (a[i]);
  if (!z.s1) __builtin_abort ();

  z.s2 = __builtin_strdup (a[j]);
  if (!z.s2) __builtin_abort ();

  z.c = cp ? *cp : '\0';    // { dg-bogus "\\\[-Wstringop-overflow" }
}
