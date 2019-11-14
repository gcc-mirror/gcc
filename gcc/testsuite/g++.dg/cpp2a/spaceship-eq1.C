// { dg-do run { target c++2a } }

struct D
{
  int i;
  bool operator==(const D& x) const = default; // OK, returns x.i == y.i
  bool operator!=(const D& z) const = default;  // OK, returns !(*this == z)
};

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

int main()
{
  D d{42};
  assert (d == d);
  assert (!(d != d));
}
