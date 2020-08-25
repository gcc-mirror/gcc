// { dg-additional-options {-fmodules-ts -Wno-pedantic} }

module;
# 5 __FILE__ 1

struct Type {};
bool operator==(Type const &, Type const &);

# 10 "" 2
export module Foo;
// { dg-module-cmi Foo }

export template<typename T>
bool equal (T const &x, T const &y)
{
  return x == y;
}

