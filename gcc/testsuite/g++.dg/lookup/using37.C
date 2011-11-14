// PR c++/30195
// { dg-do run }

template<class T> struct B
{
    void foo(T) {}
};

template<class T>
struct D : B<int>, B<double>
{
  using B<int>::foo;
  using B<double>::foo;
  void bar() { foo(3); }
};

int main()
{
  D<int> x;
  x.bar();
  return 0;
}
