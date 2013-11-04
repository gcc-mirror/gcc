// PR c++/53096
// { dg-options -std=c++11 }

struct foo
{
  foo(foo&) = default;
  foo& operator=(foo&) = default;
};
