void fn1 (void *);
void fn2 (void *);
void foo (void);
void bar (void);

extern inline void *
baz (void)
{
  return 0;
}

void
foo (void)
{
  fn1 (baz ());
  fn2 (baz ());
}

void
bar (void)
{
  fn1 (baz ());
  fn2 (baz);
}
