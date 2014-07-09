// { dg-do compile { target c++11 } }

#define BAZ "baz"

#if 0

"bar"BAZ

R"(
  bar
)"BAZ

#endif
