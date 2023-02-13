// PR tree-optimization/108166
// { dg-do run }

bool a, b;
int d, c;

const int &
foo (const int &f, const int &g)
{
  return !f ? f : g;
}

__attribute__((noipa)) void
bar (int)
{
}

int
main ()
{
  c = foo (b, 0) > ((b ? d : b) ?: 8);
  a = b ? d : b;
  bar (a);
  if (a != 0)
    __builtin_abort ();
}
