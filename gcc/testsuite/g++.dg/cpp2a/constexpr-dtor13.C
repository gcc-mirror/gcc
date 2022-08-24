// PR c++/105529
// { dg-do compile { target c++20 } }
// { dg-options "-O" }

struct allocator {
  constexpr ~allocator() {}
};
struct S {
  S(int, int, allocator = allocator());
};
void to_string() { S(0, '\0'); }
