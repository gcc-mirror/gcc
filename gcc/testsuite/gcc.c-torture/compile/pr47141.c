int
foo (__UINTPTR_TYPE__ x)
{
  int a = 6;
  int *b = &a;
  if (x)
    for (a = 0; a; a++)
      ;
  return a;
}

void
bar (void)
{
  foo ((__UINTPTR_TYPE__) foo);
}
