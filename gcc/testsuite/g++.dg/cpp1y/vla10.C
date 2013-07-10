// PR c++/57402
// { dg-options "-std=c++1y -pedantic-errors" }

int i = 2;

int main()
{
  {
    int a[i];
    a[1] = 0xbeef;
  }
  {
    int a[i] = { 1 };
    if (a[1] != 0)
      __builtin_abort ();
    a[1] = 0xbeef;
  }
  {
    int a[i] = { };
    if (a[1] != 0)
      __builtin_abort ();
    a[1] = 0xbeef;
  }
}
