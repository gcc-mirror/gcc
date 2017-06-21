// PR sanitizer/81125
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

#ifdef __SIZEOF_INT128__
typedef __int128 T;
#else
typedef long long int T;
#endif

struct A
{
  A (long);
  T a;
};

A::A (long c)
{
  long b = a % c;
}
