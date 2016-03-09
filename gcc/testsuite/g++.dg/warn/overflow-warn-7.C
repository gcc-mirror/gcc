// PR c/62096 - unexpected warning overflow in implicit constant conversion
// { dg-do compile { target c++11 } }

enum E {
    E_val  = 1,
};

inline constexpr E operator~(E e)
{
  return E(~static_cast<int>(e));
}

int main()
{
  int val = ~E_val;   // { dg-bogus "overflow in implicit constant conversion" }
  (void) val;
}
