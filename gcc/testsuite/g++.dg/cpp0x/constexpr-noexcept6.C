// PR c++/51305
// { dg-options -std=c++11 }

constexpr bool ok() noexcept
{
  typedef int type;
  return true;
}

constexpr auto x = ok();
