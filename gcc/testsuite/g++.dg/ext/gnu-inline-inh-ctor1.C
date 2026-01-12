// PR c++/123526
// { dg-do link { target c++26 } }
// { dg-options "-O0" }
// { dg-additional-sources "gnu-inline-inh-ctor2.C" }

struct A {
  [[__gnu__::__gnu_inline__]]
  constexpr explicit A (int) {}
  [[__gnu__::__gnu_inline__]]
  constexpr virtual ~A () {}
};
struct B : A {
  using A::A;
};
constexpr B b (42);
int c = 42;
B d (c);

int
main ()
{
}
