int fn2(int);
int fn3(int);

__attribute__((flatten))
int fn1(int p1)
{
  int a = fn2(p1);
  return fn3(a);
}
__attribute__((flatten))
int fn4(int p1)
{
  int j = fn2(p1);
  return fn3(j);
}

