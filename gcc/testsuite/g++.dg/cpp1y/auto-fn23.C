// PR c++/58561
// { dg-do compile { target c++1y } }
// { dg-options "-g" }

auto foo();

namespace N
{
  using ::foo;
}
