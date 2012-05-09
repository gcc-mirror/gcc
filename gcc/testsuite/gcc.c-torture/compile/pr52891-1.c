/* PR tree-optimizations/52891 */

struct S
{
  int a;
  struct T { unsigned c : 10; } b;
} s;

void
bar (short x, short y, int **p)
{
  if ((x && y) + **p)
    while (1);
}

void
foo (int **p)
{
  bar (s.a, s.b.c, p);
}
