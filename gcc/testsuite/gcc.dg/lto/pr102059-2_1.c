extern int foo1 (int *b);

int foo2 (int *b)
{
  int res = foo1 (b);
  *b += res;
  return *b;
}

