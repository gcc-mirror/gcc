// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<typename T>
struct Z {};

struct S {
  static constexpr auto r = ^^Z;
};

template<typename T, auto R>
void
g ()
{
 typename [: R :]<int> c1;
 typename [: T::r :]<int> c2;
}

void
f ()
{
  g<S, ^^Z>();
}
