// PR c++/78897
// { dg-do compile { target c++14 } }

struct Optional {
  constexpr Optional() : _dummy{} { _value = 1; }
  union {
    int _dummy;
    int _value;
  };
};
Optional opt{};
