// PR c++/57827
// { dg-do compile { target c++11 } }

struct C
{
  constexpr int fun (int x)
  {
    return x + 1;
  }

  int a = 2;
  int b = fun(a);
};

C c;
