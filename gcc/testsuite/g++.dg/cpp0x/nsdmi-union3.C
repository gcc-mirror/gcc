// PR c++/58965
// { dg-require-effective-target c++11 }

void foo()
{
  static union
  {
    int i = i;
  };
}
