// P0847R7
// { dg-do compile { target c++23 } }

// user defined copy/move assignment operators

inline constexpr int add_when_copy = 5;
inline constexpr int add_when_move = 10;
inline constexpr int poison = -1;

struct S {
  int _v;
  S& operator=(this S& self, S const& rhs) {
    self._v = rhs._v + add_when_copy;
    return self;
  };
  S& operator=(this S& self, S&& rhs) {
    self._v = rhs._v + add_when_move;
    rhs._v = poison;
    return self;
  }
};

inline constexpr int init_val = 5;

int main()
{
  S s0{0};
  S s1{init_val};

  // Sanity check.
  if (s0._v != 0
      || s1._v != init_val)
    __builtin_abort ();

  s0 = s1;
  if (s0._v != init_val + add_when_copy)
    __builtin_abort ();
  if (s1._v != init_val)
    __builtin_abort ();

  s0 = S{init_val};
  if (s0._v != init_val + add_when_move)
    __builtin_abort ();

  S s2{init_val};
  s0 = static_cast<S&&>(s2);
  if (s0._v != init_val + add_when_move)
    __builtin_abort (); 
  if (s2._v != poison)
    __builtin_abort ();
}
