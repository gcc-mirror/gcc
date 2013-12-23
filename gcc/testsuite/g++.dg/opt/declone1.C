// { dg-options "-fdeclone-ctor-dtor -O3" }

struct V {};

template <class T>
struct A: virtual V {
  static A* p;
  A();
};

template <class T>
A<T>::A()
{
  if (!p)
    p = new A();
}

int main()
{
  A<int> a;
}
