// PR c++/69736
// { dg-do compile { target c++14 } }

void fn1(bool = true)
{
  (fn1)();
}

template <typename T>
void fn2(T a = true)
{
  (fn1)();
}

void foo ()
{
  (fn2<bool>)();
}

struct X
{
  static void fn3(bool = true)
  {
    (X::fn3)();
  }

  void fn4(bool = true)
  {
    (X::fn4)();
  }
};
