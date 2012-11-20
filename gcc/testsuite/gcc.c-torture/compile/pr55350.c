void
foo (__INTPTR_TYPE__ x, __INTPTR_TYPE__ y)
{
  int i;
  void **a = (void *)  (8UL * (x / 8UL));
  for (i = 0; i < x; i++)
    a[i] = (void *) y;
}
