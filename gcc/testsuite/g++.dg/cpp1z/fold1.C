// { dg-do run }
// { dg-options "-std=c++1z" }

#include <cassert>

// Check the semantics of a couple of operations to make sure
// that the expressions are formed correctly.

#define COMMA ,

#define MAKE_FNS(name, op) \
  template<typename... Ts> \
    auto unary_left_ ## name (Ts... ts) { return (... op ts); } \
  template<typename... Ts> \
    auto unary_right_ ## name (Ts... ts) { return (ts op ...); } \
  template<typename T, typename... Ts> \
    auto binary_left_ ## name (T x, Ts... ts) { return (x op ... op ts); } \
  template<typename T, typename... Ts> \
    auto binary_right_ ## name (T x, Ts... ts) { return (ts op ... op x); }

MAKE_FNS (add, +);
MAKE_FNS (sub, -);

int main() {
  // assert(unary_left_add() == 0);
  assert(unary_left_add(1) == 1);
  assert(unary_left_add(1, 2, 3) == 6);

  // assert(unary_right_add() == 0);
  assert(unary_right_add(1) == 1);
  assert(unary_right_add(1, 2, 3) == 6);

  assert(binary_left_add(1) == 1);
  assert(binary_left_add(1, 1) == 2);
  assert(binary_left_add(1, 1, 2, 3) == 7);

  assert(binary_right_add(1) == 1);
  assert(binary_right_add(1, 1) == 2);
  assert(binary_right_add(1, 1, 2, 3) == 7);

  // unary_left_sub(); // { dg-error "empty"}
  assert(unary_left_sub(1) == 1);
  assert(unary_left_sub(1, 2, 3) == -4);

  // unary_right_sub(); // { dg-error "empty"}
  assert(unary_right_sub(1) == 1);
  assert(unary_right_sub(1, 2, 3) == 2);

  assert(binary_left_sub(1) == 1);
  assert(binary_left_sub(1, 1) == 0);
  assert(binary_left_sub(1, 1, 2, 3) == -5);

  assert(binary_right_sub(1) == 1);
  assert(binary_right_sub(1, 1) == 0);
  assert(binary_right_sub(1, 1, 2, 3) == 1);
}
