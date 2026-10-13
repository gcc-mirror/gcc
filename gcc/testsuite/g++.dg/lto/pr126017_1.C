// PR c++/126017

#include <meta>

extern std::meta::info a;

consteval std::meta::info
bar ()
{
  struct S;
  consteval { define_aggregate (^^S, { data_member_spec (^^int, { .name = "abc" }) }); }
  return ^^S;
}

struct T : [: bar () :] {};

std::meta::info
foo (std::meta::info x)
{
  T a;
  return x;
}
