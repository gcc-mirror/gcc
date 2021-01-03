// PR c++/97846
// { dg-do compile { target c++14 } }

constexpr int
f ()
{
x: // { dg-error "label definition is not a constant expression" }
  return 42;
}
