// { dg-do compile { target c++11 } }

struct A
{
  void f();
};

using ftype = decltype(&A::f);

template <class T>
bool f()
{
  ftype p = ftype{};
  return p;
}

int main()
{
  f<int>();
}
