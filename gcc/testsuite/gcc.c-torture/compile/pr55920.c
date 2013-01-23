/* PR tree-optimization/55920 */

struct A { unsigned a; } __attribute__((packed));
struct B { int b; unsigned char c[16]; };
void bar (struct A);

void
foo (struct B *x)
{
  struct A a;
  if (x->b)
    __builtin_memcpy (&a, x->c, sizeof a);
  else
    a.a = 0;
  bar (a);
}
