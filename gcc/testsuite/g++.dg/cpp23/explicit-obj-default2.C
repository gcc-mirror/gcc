// P0847R7
// { dg-do run { target c++23 } }

// defaulted copy/move assignment operators

inline constexpr int add_when_copy = 5;
inline constexpr int add_when_move = 10;
inline constexpr int poison = -1;

struct A {
  int _v;
  A(int v) : _v(v) {}
  A& operator=(A const& rhs) {
    if (&rhs == this)
      return *this;
    _v = rhs._v + add_when_copy;
    return *this;
  }
  A& operator=(A&& rhs) {
    if (&rhs == this)
      return *this;
    _v = rhs._v + add_when_move;
    rhs._v = poison;
    return *this;
  }
};

struct S {
  A _a;
  S& operator=(this S&, S const&) = default;
  S& operator=(this S&, S&&) = default;

  int v() const { return _a._v; }
};

inline constexpr int init_val = 5;

int main()
{
  S s0{0};
  S s1{init_val};

  // Sanity check.
  if (s0.v () != 0
      || s1.v () != init_val)
    __builtin_abort ();

  s0 = s1;
  if (s0.v () != init_val + add_when_copy)
    __builtin_abort ();
  if (s1.v () != init_val)
    __builtin_abort ();

  s0 = S{init_val};
  if (s0.v () != init_val + add_when_move)
    __builtin_abort ();

  S s2{init_val};
  s0 = static_cast<S&&>(s2);
  if (s0.v () != init_val + add_when_move)
    __builtin_abort (); 
  if (s2.v () != poison)
    __builtin_abort ();
}

