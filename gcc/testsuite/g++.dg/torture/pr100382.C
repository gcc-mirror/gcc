// { dg-do run }

int x, y;
int __attribute__((pure,noinline)) foo () { if (x) throw 1; return y; }

int __attribute__((noinline)) bar()
{
  int a[2];
  x = 1;
  try {
    int res = foo ();
    a[0] = res;
  } catch (...) {
      return 0;
  }
  return 1;
}

int main()
{
  if (bar ())
    __builtin_abort ();
  return 0;
}
