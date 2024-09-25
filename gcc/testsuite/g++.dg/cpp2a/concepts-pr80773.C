// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<typename F>
concept FCallable =
  requires(F)
  {
    F::f();
  };


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

