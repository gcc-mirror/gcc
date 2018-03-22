// PR c++/57402
// { dg-do run }
// { dg-options "" }
// { dg-require-effective-target alloca }

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
