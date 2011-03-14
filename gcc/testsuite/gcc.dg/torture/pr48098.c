/* { dg-do compile } */

void
foo (int n)
{
  static char *__restrict *p;
  int i;
  p = __builtin_malloc (n);
  for (i = 0; i < n; i++)
    p[i] = 0;
}

