// PR c++/53096
// { dg-do compile { target c++11 } }

struct foo
{
  foo(foo&) = default;
  foo& operator=(foo&) = default;
};
