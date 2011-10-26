// { dg-do run }
// { dg-options "-std=c++0x" }

//  Make sure embedded quotes are not a problem for string and char literals.

#include <cstdint>
#include <cassert>

int operator"" _embedchar(char)
{ return 41; };

int operator"" _embedstr(const char*, std::size_t len)
{ return 42 + len; };

void
test()
{
  int i = '\''_embedchar;

  int j = "\""_embedstr;
  assert(j == 43);

  int k = "foo\""_embedstr;
  assert(k == 46);

  int l = "\"bar"_embedstr;
  assert(l == 46);
}

int
main()
{
  test();
}
