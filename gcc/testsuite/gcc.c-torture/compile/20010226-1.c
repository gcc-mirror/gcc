void f1 (void *);
void f3 (void *, void (*)(void *));
void f2 (void *);

int foo (void *a, int b)
{
  if (!b)
    {
      f1 (a);
      return 1;
    }
  if (b)
    {
      void bar (void *c)
      {
	if (c == a)
	  f2 (c);
      }
      f3 (a, bar);
    }
  return 0;
}
