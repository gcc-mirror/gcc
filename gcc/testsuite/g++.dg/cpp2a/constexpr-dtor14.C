// PR c++/105529
// { dg-do compile { target c++20 } }
// { dg-options "-O" }
// Like constexpr-dtor13.C, except that allocator is not an empty class.

struct allocator {
  constexpr ~allocator() {}
  int a;
};
struct S {
  S(int, int, allocator = allocator());
};
void to_string() { S(0, '\0'); }
