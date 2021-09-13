// PR c++/98994
// { dg-do compile { target c++20 } }

struct empty {};

union U {
  constexpr U(): a() { }

  [[no_unique_address]] empty a;
};

constexpr U u;
