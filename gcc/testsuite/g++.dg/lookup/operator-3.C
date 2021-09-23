// PR c++/51577

template <class T> void f (T x) {
  +x; // { dg-error "no match" }
  -x; // { dg-error "no match" }
  *x; // { dg-error "no match" }
  ~x; // { dg-error "no match" }
  &x;
  !x; // { dg-error "no match" }
  ++x; // { dg-error "no match" }
  --x; // { dg-error "no match" }
  x++; // { dg-error "declared for postfix" }
  x--; // { dg-error "declared for postfix" }

  x->*x; // { dg-error "no match" }
  x / x; // { dg-error "no match" }
  x * x; // { dg-error "no match" }
  x + x; // { dg-error "no match" }
  x - x; // { dg-error "no match" }
  x % x; // { dg-error "no match" }
  x & x; // { dg-error "no match" }
  x | x; // { dg-error "no match" }
  x ^ x; // { dg-error "no match" }
  x << x; // { dg-error "no match" }
  x >> x; // { dg-error "no match" }
  x && x; // { dg-error "no match" }
  x || x; // { dg-error "no match" }
  x, x;

  x == x; // { dg-error "no match" }
  x != x; // { dg-error "no match" }
  x < x; // { dg-error "no match" }
  x > x; // { dg-error "no match" }
  x <= x; // { dg-error "no match" }
  x >= x; // { dg-error "no match" }
#if __cplusplus > 201703L
  x <=> x; // { dg-error "no match" "" { target c++20 } }
#endif

  x += x; // { dg-error "no match" }
  x -= x; // { dg-error "no match" }
  x *= x; // { dg-error "no match" }
  x /= x; // { dg-error "no match" }
  x %= x; // { dg-error "no match" }
  x |= x; // { dg-error "no match" }
  x ^= x; // { dg-error "no match" }
  x <<= x; // { dg-error "no match" }
  x >>= x; // { dg-error "no match" }
}

namespace N { struct A { }; }

void operator+(N::A);
void operator-(N::A);
void operator*(N::A);
void operator~(N::A);
#if __cplusplus >= 201103L
void operator&(N::A) = delete;
#else
void operator&(N::A);
#endif
void operator!(N::A);
void operator++(N::A);
void operator--(N::A);
void operator++(N::A, int);
void operator--(N::A, int);

void operator->*(N::A, N::A);
void operator/(N::A, N::A);
void operator*(N::A, N::A);
void operator+(N::A, N::A);
void operator-(N::A, N::A);
void operator%(N::A, N::A);
void operator&(N::A, N::A);
void operator|(N::A, N::A);
void operator^(N::A, N::A);
void operator<<(N::A, N::A);
void operator>>(N::A, N::A);
void operator&&(N::A, N::A);
void operator||(N::A, N::A);
#if __cplusplus >= 201103L
void operator,(N::A, N::A) = delete;
#else
void operator,(N::A, N::A);
#endif

void operator==(N::A, N::A);
void operator!=(N::A, N::A);
void operator<(N::A, N::A);
void operator>(N::A, N::A);
void operator<=(N::A, N::A);
void operator>=(N::A, N::A);
#if __cplusplus > 201703L
void operator<=>(N::A, N::A);
#endif

void operator+=(N::A, N::A);
void operator-=(N::A, N::A);
void operator*=(N::A, N::A);
void operator/=(N::A, N::A);
void operator%=(N::A, N::A);
void operator|=(N::A, N::A);
void operator^=(N::A, N::A);
void operator<<=(N::A, N::A);
void operator>>=(N::A, N::A);

int main() {
  f(N::A());
}
