// PR c++/97846
// { dg-do compile { target c++14 } }

constexpr int
f ()
{
x: // { dg-error "label definition in 'constexpr' function only available with" "" { target c++20_down } }
  return 42;
}
