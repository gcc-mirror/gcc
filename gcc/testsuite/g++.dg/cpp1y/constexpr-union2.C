// { dg-do compile { target c++14 } }

union U
{
  char *x = &y;
  char y;
};

constexpr U u = {};
