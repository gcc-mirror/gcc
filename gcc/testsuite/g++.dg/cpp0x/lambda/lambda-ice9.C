// PR c++/53741
// { dg-do compile { target c++11 } }

struct X
{
  template <class T> static void bar() {}

  template <class T> void foo(T p) 
  {
    [&] { bar<T>(); };
  }
};

int main()
{
  X x;
  x.foo(3);
}
