/* PR tree-optimizations/52891 */

struct __attribute__((packed)) S { unsigned s : 22; };
struct __attribute__((packed)) T { struct S t; } c;
int a, b, d;

void
foo (void)
{
  if (1 ? (!c.t.s & (d < 0)) < a : 0)
    b = 0;
}
