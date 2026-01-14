// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

namespace N { }

template<info R>
void
f ()
{
  int i = [:R:]; // { dg-error "expected a reflection of an expression instead of .N." }
}

void
g ()
{
  f<^^N>();
}
