// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++20 } }
// In c++2a we don't emit a call to _ZN3FooI3ArgEC1Ev.

struct Arg;
struct Base {
  int i;
  virtual ~Base();
};
template <class> struct Foo : Base { };
Foo<Arg> a;
