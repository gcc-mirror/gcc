// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

struct S {
  template<typename T>
  static T t{};
} s;

template<info R>
void
f ()
{
  int j = s.template [:R:]<int>; // { dg-error "expected a reflection of an expression instead of type .S." }
}

void
g ()
{
  f<^^S>();
}
