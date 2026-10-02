// PR c++/127551
// { dg-do run { target c++17 } }

#include <string_view>

int
main ()
{
  constexpr std::string_view test = "hello";
  auto pos = test.find_first_of('l', 2);
  if (pos != 2)
    __builtin_abort ();

  std::string_view test2 = "hello";
  auto pos2 = test2.find_first_of('l', 2);
  if (pos2 != 2)
    __builtin_abort ();
}
