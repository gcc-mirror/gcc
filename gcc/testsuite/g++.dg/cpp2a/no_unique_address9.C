// PR c++/97566
// { dg-do compile { target c++14 } }

// error disappears if E doesn't inherit from B
struct B {};
struct E : B {};

struct counter {
  constexpr void inc() { size++; }

  // error disappears if you remove or reorder this value
  int unused = 0;
  int size = 0;
  [[no_unique_address]] E empty = {};
};

#define SA(X) static_assert((X),#X)

constexpr int test1() {
  counter x;
  x.inc();
  return x.size;
}
SA(test1() == 1);

constexpr int test2() {
  counter x = { 0, 1, {} };
  x.inc();
  return x.size;
}
SA(test2() == 2);

counter y;

struct counter2 {
  constexpr counter2() { inc(); }
  constexpr void inc() { size++; }

  // error disappears if you remove or reorder this value
  int unused = 0;
  int size = 0;
  [[no_unique_address]] E empty = {};
};

constexpr int test3() {
  counter2 x;
  x.inc();
  return x.size;
}
SA(test3() == 2);
