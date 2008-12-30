struct S
{
  struct S *n, *p;
} *s;

void bar (void *);

int
foo (int x)
{
  struct S p = { &p, &p };
  int i;
  for (i = 0; i < x; i++)
    bar (s);
  return p.n == &p;
}

int dialog_calendar(int state)
{
  int *obj = (state == 1 ? &state : 0);
  return (obj == &state);
}
