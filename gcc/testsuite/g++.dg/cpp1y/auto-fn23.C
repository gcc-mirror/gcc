// PR c++/58561
// { dg-do compile { target c++14 } }
// { dg-options "-g" }

auto foo();

namespace N
{
  using ::foo;
}
