/* PR tree-optimization/84383 */

struct S { char *s; };
void bar (struct S *);

void
foo (int a, char *b)
{
  struct S c[4];
  bar (c);
  __builtin_strncpy (c[a].s, b, 32);
  c[a].s[31] = '\0';
  bar (c);
}
