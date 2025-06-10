// PR c++/120502
// { dg-do compile { target c++20 } }
// { dg-additional-options -O }

struct non_trivial_if {
  constexpr non_trivial_if() {}
};
struct allocator : non_trivial_if {};
struct padding {};
struct __short {
  [[no_unique_address]] padding p;
};
struct basic_string {
  union {
    __short s;
    int l;
  };
  [[no_unique_address]] allocator a;
  constexpr basic_string() {}
  ~basic_string() {}
};
struct time_zone {
  basic_string __abbrev;
  long __offset;
};
time_zone convert_to_time_zone() { return {}; }
