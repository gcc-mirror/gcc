// PR c++/67445
// { dg-do compile { target c++11 } }

#include <initializer_list>

using SL = std::initializer_list<char const*>;

SL retArray(int i) noexcept
{
  if (i == 0)
    {
      SL l{"Test 1", "Test 2", "Test 3"}; // { dg-message "declared" }
      return l;			// { dg-warning "initializer_list" }
    }
  else if (i == 1)
    return SL{"Test 1", "Test 2", "Test 3"}; // { dg-warning "initializer_list" }
  else if (i == 2)
    return {"Test 1", "Test 2", "Test 3"}; // { dg-warning "initializer_list" }
  else
    {
      static SL l{"Test 1", "Test 2", "Test 3"};
      return l;			// no warning about returning static.
    }
}

const char *p;
int main(int, char const* const*)
{
  for (auto&& i : retArray(1))
    {
      p = i;
    }
  return 0;
}
