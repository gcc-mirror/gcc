// PR c++/96311
// { dg-do compile { target c++14 } }
// { dg-additional-options -Wunused }

auto foo()
{
  constexpr int used = 0;
  return
    [](auto unused)
    {
      return used;
    };
}

int main()
{
  foo()(42);
}
