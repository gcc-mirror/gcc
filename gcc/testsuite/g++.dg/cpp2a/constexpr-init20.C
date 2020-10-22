// PR c++/97328
// { dg-do compile { target c++20 } }

struct vector {
  union storage {
    int t;
    constexpr storage() {}
  } data[8];
};

constexpr auto foo() {
  vector i;
  return i;
}
auto val = foo();
