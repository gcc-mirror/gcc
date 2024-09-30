// PR c++/116162
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-defaulted-function-deleted" }

struct C0 {
  C0(C0&) = default;
};

struct C1 {
  C1(volatile C1&) = default;
};

struct C2 {
  C2(const C2&) = default;
};

struct C3 {
  C3(const volatile C3&) = default;
};

struct M0 {
  M0(M0&&) = default;
};

struct M1 {
  M1(const M1&&) = default;
};

struct M2 {
  M2(volatile M2&&) = default;
};

struct M3 {
  M3(const volatile M3&&) = default;
};
