// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template<typename F>
concept bool FCallable()
{
  return requires(F)
  {
      F::f();
  };
}

class Test1
{
public:
  template<FCallable P, FCallable... Pp>
  static void g()
  {
    (Pp::f(), ...);
  }
};

class A
{
public:
  static void f() {}
};

template<typename X> concept bool C = true;

template<C... X>
void bar(X...)
{}

struct foo
{
  template<C... X>
  void bar(X...)
  {}
};

int main()
{
  Test1::template g<A>();
  bar();
  foo {}.bar();
}

