// PR c++/94462
// { dg-do compile { target c++20 } }

namespace std {
  struct strong_ordering { };
}

namespace Synth {
  struct B {
    friend std::strong_ordering operator<=>(B, B) = default;
  };

  struct C {
    friend bool operator==(C, C);
  };
}

