/* PR sanitizer/56330 */
/* { dg-do compile } */

char e[200];

struct S
{
  char a[100];
  char b[100];
} s;

int
foo (void)
{
  int i = __builtin_memcmp (s.a, e, 100);
  i += __builtin_memcmp (s.a, e, 200);
  return i;
}

void
bar (int *a, char *b, char *c)
{
  __builtin_memmove (c, b, a[b[0]]);
}
