// { dg-do compile { target c++11 } }

template <typename To, typename From>
constexpr To
bit_cast (const From &from)
{
  return __builtin_bit_cast (To, from);
}

template <typename To, typename From>
constexpr bool
check (const From &from)
{
  return bit_cast <From> (bit_cast <To> (from)) == from;
}

struct A
{
  int a, b, c;
  constexpr bool operator == (const A &x) const
  {
    return x.a == a && x.b == b && x.c == c;
  }
};

struct B
{
  unsigned a[3];
  constexpr bool operator == (const B &x) const
  {
    return x.a[0] == a[0] && x.a[1] == a[1] && x.a[2] == a[2];
  }
};

struct C
{
  char a[2][3][2];
  constexpr bool operator == (const C &x) const
  {
    return x.a[0][0][0] == a[0][0][0]
	   && x.a[0][0][1] == a[0][0][1]
	   && x.a[0][1][0] == a[0][1][0]
	   && x.a[0][1][1] == a[0][1][1]
	   && x.a[0][2][0] == a[0][2][0]
	   && x.a[0][2][1] == a[0][2][1]
	   && x.a[1][0][0] == a[1][0][0]
	   && x.a[1][0][1] == a[1][0][1]
	   && x.a[1][1][0] == a[1][1][0]
	   && x.a[1][1][1] == a[1][1][1]
	   && x.a[1][2][0] == a[1][2][0]
	   && x.a[1][2][1] == a[1][2][1];
  }
};

struct D
{
  int a, b;
  constexpr bool operator == (const D &x) const
  {
    return x.a == a && x.b == b;
  }
};

struct E {};
struct F { char c, d, e, f; };
struct G : public D, E, F
{
  int g;
  constexpr bool operator == (const G &x) const
  {
    return x.a == a && x.b == b && x.c == c && x.d == d
	   && x.e == e && x.f == f && x.g == g;
  }
};

struct H
{
  int a, b[2], c;
  constexpr bool operator == (const H &x) const
  {
    return x.a == a && x.b[0] == b[0] && x.b[1] == b[1] && x.c == c;
  }
};

#if __SIZEOF_INT__ == 4
struct I
{
  int a;
  int b : 3;
  int c : 24;
  int d : 5;
  int e;
  constexpr bool operator == (const I &x) const
  {
    return x.a == a && x.b == b && x.c == c && x.d == d && x.e == e;
  }
};
#endif

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
struct J
{
  long long int a, b : 11, c : 3, d : 37, e : 1, f : 10, g : 2, h;
  constexpr bool operator == (const J &x) const
  {
    return x.a == a && x.b == b && x.c == c && x.d == d && x.e == e
	   && x.f == f && x.g == g && x.h == h;
  }
};

struct K
{
  long long int a, b, c;
  constexpr bool operator == (const K &x) const
  {
    return x.a == a && x.b == b && x.c == c;
  }
};

struct M
{
  signed a : 6, b : 7, c : 6, d : 5;
  unsigned char e;
  unsigned int f;
  long long int g;
  constexpr bool operator == (const M &x) const
  {
    return x.a == a && x.b == b && x.c == c && x.d == d && x.e == e
	   && x.f == f && x.g == g;
  }
};

struct N
{
  unsigned long long int a, b;
  constexpr bool operator == (const N &x) const
  {
    return x.a == a && x.b == b;
  }
};
#endif

static_assert (check <unsigned int> (0), "");
static_assert (check <long long int> (0xdeadbeeffeedbac1ULL), "");
static_assert (check <signed char> ((unsigned char) 42), "");
static_assert (check <char> ((unsigned char) 42), "");
static_assert (check <unsigned char> ((unsigned char) 42), "");
static_assert (check <signed char> ((signed char) 42), "");
static_assert (check <char> ((signed char) 42), "");
static_assert (check <unsigned char> ((signed char) 42), "");
static_assert (check <signed char> ((char) 42), "");
static_assert (check <char> ((char) 42), "");
static_assert (check <unsigned char> ((char) 42), "");
#if __SIZEOF_INT__ == __SIZEOF_FLOAT__
static_assert (check <int> (2.5f), "");
static_assert (check <unsigned int> (136.5f), "");
#endif
#if __SIZEOF_LONG_LONG__ == __SIZEOF_DOUBLE__
static_assert (check <long long> (2.5), "");
static_assert (check <long long unsigned> (123456.75), "");
#endif

static_assert (check <B> (A{ 1, 2, 3 }), "");
static_assert (check <A> (B{ 4, 5, 6 }), "");

#if __SIZEOF_INT__ == 4
static_assert (check <C> (A{ 7, 8, 9 }), "");
static_assert (check <C> (B{ 10, 11, 12 }), "");
static_assert (check <A> (C{ { { { 13, 14 }, { 15, 16 }, { 17, 18 } },
			       { { 19, 20 }, { 21, 22 }, { 23, 24 } } } }), "");
constexpr unsigned char c[] = { 1, 2, 3, 4 };
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
static_assert (bit_cast <unsigned int> (c) == 0x04030201U, "");
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static_assert (bit_cast <unsigned int> (c) == 0x01020304U, "");
#endif

#if __cplusplus >= 201703L
static_assert (check <G> (H { 0x12345678, { 0x23456789, 0x5a876543 }, 0x3ba78654 }), "");
#endif
constexpr int d[] = { 0x12345678, 0x23456789, 0x5a876543, 0x3ba78654 };
static_assert (bit_cast <G> (d) == bit_cast <G> (H { 0x12345678, { 0x23456789, 0x5a876543 }, 0x3ba78654 }), "");

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
static_assert (bit_cast <I> (A { 0x7efa3412, 0x5a876543, 0x1eeffeed })
	       == I { 0x7efa3412, 3, 0x50eca8, 0xb, 0x1eeffeed }, "");
static_assert (bit_cast <A> (I { 0x7efa3412, 3, 0x50eca8, 0xb, 0x1eeffeed })
	       == A { 0x7efa3412, 0x5a876543, 0x1eeffeed }, "");
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static_assert (bit_cast <I> (A { 0x7efa3412, 0x5a876543, 0x1eeffeed })
	       == I { 0x7efa3412, 2, -0x2bc4d6, 0x3, 0x1eeffeed }, "");
static_assert (bit_cast <A> (I { 0x7efa3412, 2, -0x2bc4d6, 0x3, 0x1eeffeed })
	       == A { 0x7efa3412, 0x5a876543, 0x1eeffeed }, "");
#endif
#endif

#if 2 * __SIZEOF_INT__ == __SIZEOF_LONG_LONG__ && __SIZEOF_INT__ >= 4
constexpr unsigned long long a = 0xdeadbeeffee1deadULL;
constexpr unsigned b[] = { 0xfeedbacU, 0xbeeffeedU };
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
static_assert (bit_cast <D> (a) == D { int (0xfee1deadU), int (0xdeadbeefU) }, "");
static_assert (bit_cast <unsigned long long> (b) == 0xbeeffeed0feedbacULL, "");
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static_assert (bit_cast <D> (a) == D { int (0xdeadbeefU), int (0xfee1deadU) }, "");
static_assert (bit_cast <unsigned long long> (b) == 0x0feedbacbeeffeedULL, "");
#endif
#endif

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
static_assert (bit_cast <J> (K { 0x0feedbacdeadbeefLL, 7862463375103529997LL, 0x0feedbacdeadbeefLL })
	       == J { 0x0feedbacdeadbeefLL, -1011, 2, -0xbacdeadbeLL, -1, -303, 1, 0x0feedbacdeadbeefLL }, "");
static_assert (bit_cast <K> (J { 0x0feedbacdeadbeefLL, -1011, 2, -0xbacdeadbeLL, -1, -303, 1, 0x0feedbacdeadbeefLL })
	       == K { 0x0feedbacdeadbeefLL, 7862463375103529997LL, 0x0feedbacdeadbeefLL }, "");
static_assert (bit_cast <M> (N { 0xfeedbacdeadbeef8ULL, 0x123456789abcde42ULL })
	       == M { -8, 59, 31, -5, 234, 0xfeedbacdU, 0x123456789abcde42ULL }, "");
static_assert (bit_cast <N> (M { -8, 59, 31, -5, 234, 0xfeedbacdU, 0x123456789abcde42ULL })
	       == N { 0xfeedbacdeadbeef8ULL, 0x123456789abcde42ULL }, "");
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static_assert (bit_cast <J> (K { 0x0feedbacdeadbeefLL, -9103311533965288635LL, 0x0feedbacdeadbeefLL })
	       == J { 0x0feedbacdeadbeefLL, -1011, 2, -0xbacdeadbeLL, -1, -303, 1, 0x0feedbacdeadbeefLL }, "");
static_assert (bit_cast <K> (J { 0x0feedbacdeadbeefLL, -1011, 2, -0xbacdeadbeLL, -1, -303, 1, 0x0feedbacdeadbeefLL })
	       == K { 0x0feedbacdeadbeefLL, -9103311533965288635LL, 0x0feedbacdeadbeefLL }, "");
static_assert (bit_cast <M> (N { 0xfeedbacdeadbeef8ULL, 0x123456789abcde42ULL })
	       == M { -1, -35, -19, -6, 205, 0xeadbeef8U, 0x123456789abcde42ULL }, "");
static_assert (bit_cast <N> (M { -1, -35, -19, -6, 205, 0xeadbeef8U, 0x123456789abcde42ULL })
	       == N { 0xfeedbacdeadbeef8ULL, 0x123456789abcde42ULL }, "");
#endif
#endif
