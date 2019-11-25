// P0428R2
// { dg-do compile { target c++14 } }

struct S { int s; };

auto x =
#if __cpp_generic_lambdas >= 201707
  []<class T = S>(T &&t) { return t.s; } ({ 2 });
#else
  [](auto &&t) { return t.s; } (S { 2 });
#endif
