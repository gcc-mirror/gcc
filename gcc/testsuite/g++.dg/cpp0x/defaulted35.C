// PR c++/53096
// { dg-options -std=c++0x }

struct foo
{
  foo(foo&) = default;
  foo& operator=(foo&) = default;
};
