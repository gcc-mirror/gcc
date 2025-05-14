/* PR middle-end/87099 */
/* { dg-do compile } */
/* { dg-options "-Wstringop-overflow" } */

void bar (char *);

int
foo (int n)
{
  char v[n];
  bar (v);
  return __builtin_strncmp (&v[1], "aaa", 3);
}

int
baz (int n, char *s)
{
  char v[n];
  bar (v);
  return __builtin_strncmp (&v[1], s, 3);
}
