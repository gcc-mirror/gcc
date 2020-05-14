// { dg-do run { target c++20 } }

template <class T>
struct D
{
  T i;
  bool operator==(const D& x) const = default; // OK, returns x.i == y.i
  bool operator!=(const D& z) const = default;  // OK, returns !(*this == z)
};

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

template <class T>
void f()
{
  D<T> d{42};
  assert (d == d);
  assert (!(d != d));
}

int main()
{
  f<int>();
}
