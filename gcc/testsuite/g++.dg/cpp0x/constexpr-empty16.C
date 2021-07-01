// PR c++/101194
// { dg-do compile { target c++11 } }

struct nodefault {
  constexpr nodefault(int) { }
};

constexpr nodefault x[1] = { nodefault{0} };

constexpr nodefault y = x[0];
