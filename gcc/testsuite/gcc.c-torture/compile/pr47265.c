/* PR tree-optimization/47265 */

struct S
{
  char a[3];
  char b[3];
};

void
bar (char *dst, const char *src, unsigned n)
{
  while (n--)
    *dst++ = *src ? *src++ : ' ';
}

void
foo (struct S *s)
{
  bar (s->a, s->b, 3);
}
