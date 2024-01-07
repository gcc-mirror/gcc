// P0847R7
// { dg-do run { target c++23 } }

// conversion operators with xobj parameter

inline constexpr int magic = 42;

struct S0 {
  operator int(this S0 const&) {
    return magic;
  }
};

struct S1 {
  int _v;
  int f(this int self) {
    return self;
  }
  operator int(this S1 const& self) {
    return self._v;
  }
};

int main()
{
  if (S0{} != magic)
    __builtin_abort ();

  S1 s{42};
  if (static_cast<int>(s) != magic)
    __builtin_abort ();
}

