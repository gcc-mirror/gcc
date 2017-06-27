// PR sanitizer/81209
// { dg-do compile }
// { dg-options "-fsanitize=undefined -fno-declone-ctor-dtor" }

#ifdef __SIZEOF_INT128__
typedef __int128 T;
#else
typedef long long int T;
#endif

struct B {};
struct A : virtual public B
{
  A (long);
  T a;
};

A::A (long c)
{
  long b = a % c;
}
