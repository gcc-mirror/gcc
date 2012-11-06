// PR c++/54922
// { dg-do compile { target c++11 } }

class nullable_int
{
  bool init_;
  union {
    unsigned char for_value_init;
    int value_;
  };
public:
  constexpr nullable_int() : init_(false), for_value_init() {}
};
