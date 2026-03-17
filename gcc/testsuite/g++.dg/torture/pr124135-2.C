// PR tree-optimization/124135
// { dg-do compile }

#ifdef __SIZEOF_FLOAT128__
void foo (char, __float128 *, int);
int qux (int);

__float128
bar (int x)
{
  __float128 a;
  if (x)
    foo (0, &a, qux (42));
  return a;
}

void
baz (int x)
{
  bar (x);
}
#else
void
baz ()
{
}
#endif
