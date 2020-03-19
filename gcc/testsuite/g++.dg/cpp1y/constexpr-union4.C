// { dg-do compile { target c++14 } }

union U
{
  int x = y;
  char y;
};

constexpr U u = {}; // { dg-error "accessing uninitialized member" }
