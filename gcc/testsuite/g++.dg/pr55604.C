/* { dg-do compile } */
/* { dg-options "-O -fdump-rtl-reload" } */

int
main ()
{
  char s[10];
  const int t = (__builtin_memcpy (s, "Hello", 6), 5);
  __builtin_printf ("%d %s\n", t, s);
  return 0;
}

