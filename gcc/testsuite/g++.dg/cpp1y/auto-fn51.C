// PR c++/85240
// { dg-do compile { target c++14 } }

auto foo();

void bar()
{
  using ::foo;
}
