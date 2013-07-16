// PR c++/54922
// { dg-do compile { target c++11 } }

struct nullable_int
{
  bool init_;
  union {
    unsigned char for_value_init;
    int value_;
  };

  constexpr nullable_int() : init_(false), for_value_init() {}
};

#define SA(X) static_assert(X,#X)

constexpr nullable_int n;
SA((n.for_value_init == 0));
