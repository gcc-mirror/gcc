/* PR other/81345 - -Wall resets -Wstringop-overflow to 1 from the default 2
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char a[3];

void f (const char *s)
{
  __builtin_strncpy (a, s, sizeof a + 1);   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

struct S { char a[3]; int i; };

void g (struct S *d, const char *s)
{
  __builtin_strncpy (d->a, s, sizeof d->a + 1);   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}
