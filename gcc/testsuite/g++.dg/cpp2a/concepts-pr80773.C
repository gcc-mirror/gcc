// { dg-do compile { target c++2a } }
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

int main()
{
  Test1::template g<A>();
}

