// { dg-do compile }
struct A { short a; };
int **b;
unsigned long c;

bool foo ();
unsigned bar (unsigned i);
extern void baz () __attribute__((noreturn));

int *
test (unsigned x, struct A *y)
{
  unsigned v;
  if (foo () || y[x].a == -1)
    {
      c = bar (x);
      return 0;
    }
  v = y[x].a;
  if (v >= 23)
    baz ();
  return b[v];
}
