// PR c++/69223
// { dg-do compile { target c++11 } }

template <class T> struct A
{
  T x[20];
};

int main()
{
  auto l = [](const A<int>& i){ return i; };
  A<int> a;

  l(a);
}
