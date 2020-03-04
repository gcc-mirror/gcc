// PR c++/90432
// { dg-do compile { target c++11 } }

struct empty {};

struct has_empty {
  [[no_unique_address]] empty brace_or_equal_initialized{};
};

struct has_value {
  int non_zero = 1;
};

struct pair : has_empty, has_value {};

pair a;
