// PR c++/58561
// { dg-options "-std=c++1y -g" }

auto foo();

namespace N
{
  using ::foo;
}
