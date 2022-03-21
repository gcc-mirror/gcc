// PR c++/103337
// { dg-do compile { target c++20 } }

struct op_t {
  struct put_t {
    int x;
  } put;
};

op_t x{0};       // OK
op_t y{.put=0};  // bogus error: 'op_t::put_t' has no non-static data member named 'put'
