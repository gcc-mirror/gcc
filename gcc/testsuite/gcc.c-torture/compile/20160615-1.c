int a;
void bar (int, unsigned, unsigned);

void
foo (unsigned x)
{
  unsigned b = a ? x : 0;
  if (x || b)
    bar (0, x, b);
}
