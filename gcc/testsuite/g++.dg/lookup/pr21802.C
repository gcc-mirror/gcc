// PR c++/21802
// { dg-do run }
#include <cassert>

struct X;
int I = 6;

/* A mostly exhaustive and ad-hoc assortment of operator overloads and calls
   thereof, to stress-test two-stage name lookup of operators inside template
   definitions and then to verify that the calls get built correctly.  */

template <typename T>
inline int operator+(const X &, T x) { return x; }
inline int operator-(const X &, int x) { return x; }
inline int operator*(const X &, int x) { return x; }
inline int operator/(const X &, int x) { return x; }
inline int operator+=(const X &, int x) { return x; }

struct X
{
  X () : m (1) { }
  template <typename T>
  int operator%(T x) { return m + x; }
  virtual int operator>>(int x) { return m + x; }
  int operator<<(int x) { return m + x; }
  int operator&(int x) { return m + x; }
  int operator|(int x) { return m + x; }
  int operator^(int x) { return m + x; }
  int operator&&(int x) { return m + x; }
  int operator||(int x) { return m + x; }
  friend int operator==(X o, int x) { return o.m + x; }
  int operator!=(int x) { return m + x; }
  int operator<(int x) { return m + x; }
  int operator<=(int x) { return m + x; }
  int operator>(int x) { return m + x; }
  int operator>=(int x) { return m + x; }
  int operator*() { return m + I; }
  int operator!() { return m + I; }
  int operator~() { return m + I; }
  int operator++() { return m + I + 100; }
  int operator--() { return m + I + 100; }
  int operator++(int) { return m + I; }
  int operator--(int) { return m + I; }
  int operator()() { return m + I; }
  int operator,(int x) { return m + x; }
  int operator[](int x) { return m + x; }
  int operator*=(int x) { return m + x; }
  int operator-=(int x) { return m + x; }
  int operator/=(int x) { return m + x; }
  virtual int operator& () { return m + I; }
  int m;
};
struct Y : virtual X
{
  /* Virtual override.  */
  int operator>>(int x) { return m + x + 1; }
  int operator& () { return m + I + 1; }

  /* Not virtual.  */
  template <typename T>
  int operator&(T x) { return m + x + 1; }
  friend int operator==(Y o, int x) { return o.m + x + 1; }
  int operator!=(int x) { return m + x + 1; }
};

/* The folloiwng "FooN" functions each contain a different way to call and to
   resolve these operator overloads.  */

template <typename T>
void
Foo1 (T)
{
  Y x;
  { int t = x + I; assert (t == 6); }
  { int t = x - I; assert (t == 6); }
  { int t = x * I; assert (t == 6); }
  { int t = x / I; assert (t == 6); }
  { int t = (x+=I); assert (t == 6); }

  { int t = x % I; assert (t == 7); }
  { int t = x << I; assert (t == 7); }
  { int t = x | I; assert (t == 7); }
  { int t = x && I; assert (t == 7); }
  { int t = x || I; assert (t == 7); }
  { int t = x < I; assert (t == 7); }
  { int t = x <= I; assert (t == 7); }
  { int t = x > I; assert (t == 7); }
  { int t = x >= I; assert (t == 7); }
  { int t = *x; assert (t == 7); }
  { int t = !x; assert (t == 7); }
  { int t = ~x; assert (t == 7); }
  { int t = x++; assert (t == 7); }
  { int t = x--; assert (t == 7); }
  { int t = ++x; assert (t == 107); }
  { int t = --x; assert (t == 107); }
  { int t = x (); assert (t == 7); }
  { int t = (x, I); assert (t == 7); }
  { int t = x[I]; assert (t == 7); }
  { int t = (x-=I); assert (t == 7); }
  { int t = (x/=I); assert (t == 7); }
  { int t = (x*=I); assert (t == 7); }

  { int t = x >> I; assert (t == 8); }
  { int t = x & I; assert (t == 8); }
  { int t = &x; assert (t == 8); }
  { int t = x == I; assert (t == 8); }
  { int t = x != I; assert (t == 8); }
}

template <typename T>
void
Foo2 (T)
{
  X x;
  { int t = x + I; assert (t == 6); }
  { int t = x - I; assert (t == 6); }
  { int t = x * I; assert (t == 6); }
  { int t = x / I; assert (t == 6); }
  { int t = (x+=I); assert (t == 6); }

  { int t = x % I; assert (t == 7); }
  { int t = x >> I; assert (t == 7); }
  { int t = x << I; assert (t == 7); }
  { int t = x | I; assert (t == 7); }
  { int t = x && I; assert (t == 7); }
  { int t = x || I; assert (t == 7); }
  { int t = x == I; assert (t == 7); }
  { int t = x != I; assert (t == 7); }
  { int t = x < I; assert (t == 7); }
  { int t = x <= I; assert (t == 7); }
  { int t = x > I; assert (t == 7); }
  { int t = x >= I; assert (t == 7); }
  { int t = *x; assert (t == 7); }
  { int t = !x; assert (t == 7); }
  { int t = ~x; assert (t == 7); }
  { int t = x++; assert (t == 7); }
  { int t = x--; assert (t == 7); }
  { int t = ++x; assert (t == 107); }
  { int t = --x; assert (t == 107); }
  { int t = x (); assert (t == 7); }
  { int t = (x, I); assert (t == 7); }
  { int t = x[I]; assert (t == 7); }
  { int t = &x; assert (t == 7); }
  { int t = (x-=I); assert (t == 7); }
  { int t = (x/=I); assert (t == 7); }
  { int t = (x*=I); assert (t == 7); }
  { int t = x & I; assert (t == 7); }
}

template <typename T>
void
Foo3 (T)
{
  Y o;
  X &x = o;
  { int t = x + I; assert (t == 6); }
  { int t = x - I; assert (t == 6); }
  { int t = x * I; assert (t == 6); }
  { int t = x / I; assert (t == 6); }
  { int t = (x+=I); assert (t == 6); }

  { int t = x % I; assert (t == 7); }
  { int t = x << I; assert (t == 7); }
  { int t = x | I; assert (t == 7); }
  { int t = x && I; assert (t == 7); }
  { int t = x || I; assert (t == 7); }
  { int t = x == I; assert (t == 7); }
  { int t = x != I; assert (t == 7); }
  { int t = x < I; assert (t == 7); }
  { int t = x <= I; assert (t == 7); }
  { int t = x > I; assert (t == 7); }
  { int t = x >= I; assert (t == 7); }
  { int t = *x; assert (t == 7); }
  { int t = !x; assert (t == 7); }
  { int t = ~x; assert (t == 7); }
  { int t = x++; assert (t == 7); }
  { int t = x--; assert (t == 7); }
  { int t = ++x; assert (t == 107); }
  { int t = --x; assert (t == 107); }
  { int t = x (); assert (t == 7); }
  { int t = (x, I); assert (t == 7); }
  { int t = x[I]; assert (t == 7); }
  { int t = (x-=I); assert (t == 7); }
  { int t = (x/=I); assert (t == 7); }
  { int t = (x*=I); assert (t == 7); }

  { int t = x & I; assert (t == 7); }
  { int t = x >> I; assert (t == 8); }
  { int t = &x; assert (t == 8); }
}

template <typename T>
void
Foo4 (T)
{
  Y x;
  { int t = operator+ (x, I); assert (t == 6); }
  { int t = operator- (x, I); assert (t == 6); }
  { int t = operator* (x, I); assert (t == 6); }
  { int t = operator/ (x, I); assert (t == 6); }
  { int t = operator+= (x, I); assert (t == 6); }

  { int t = x.operator% (I); assert (t == 7); }
  { int t = x.operator<< (I); assert (t == 7); }
  { int t = x.operator| (I); assert (t == 7); }
  { int t = x.operator&& (I); assert (t == 7); }
  { int t = x.operator|| (I); assert (t == 7); }
  { int t = x.operator< (I); assert (t == 7); }
  { int t = x.operator<= (I); assert (t == 7); }
  { int t = x.operator> (I); assert (t == 7); }
  { int t = x.operator>= (I); assert (t == 7); }
  { int t = x.operator* (); assert (t == 7); }
  { int t = x.operator! (); assert (t == 7); }
  { int t = x.operator~ (); assert (t == 7); }
  { int t = x.operator++ (0); assert (t == 7); }
  { int t = x.operator-- (0); assert (t == 7); }
  { int t = x.operator++ (); assert (t == 107); }
  { int t = x.operator-- (); assert (t == 107); }
  { int t = x.operator() (); assert (t == 7); }
  { int t = x.operator, (I); assert (t == 7); }
  { int t = x.operator[] (I); assert (t == 7); }
  { int t = x.operator-= (I); assert (t == 7); }
  { int t = x.operator/= (I); assert (t == 7); }
  { int t = x.operator*= (I); assert (t == 7); }

  { int t = x.operator>> (I); assert (t == 8); }
  { int t = x.operator& (); assert (t == 8); }
  { int t = x.operator& (I); assert (t == 8); }
  { int t = operator== (x, I); assert (t == 8); }
  { int t = x.operator!= (I); assert (t == 8); }
}


/* These definitions should be irrelevant to operator lookup of non-dependent
   expressions inside the above templates since they are not in scope at
   template-definition time (even though they are in scope at instantiation
   time).  */
inline int operator+(const Y&, int) { return 11; }
inline int operator-(const Y&, int) { return 11; }
inline int operator*(const Y&, int) { return 11; }
inline int operator/(const Y&, int) { return 11; }
inline int operator%(const Y&, int) { return 11; }
inline int operator>>(const Y&, int) { return 11; }
inline int operator<<(const Y&, int) { return 11; }
inline int operator&(const Y&, int) { return 11; }
inline int operator|(const Y&, int) { return 11; }
inline int operator^(const Y&, int) { return 11; }
inline int operator&&(const Y&, int) { return 11; }
inline int operator||(const Y&, int) { return 11; }
inline int operator==(const Y&, int) { return 11; }
inline int operator!=(const Y&, int) { return 11; }
inline int operator<(const Y&, int) { return 11; }
inline int operator<=(const Y&, int) { return 11; }
inline int operator>(const Y&, int) { return 11; }
inline int operator>=(const Y&, int) { return 11; }
inline int operator*(const Y&) { return 11; }
inline int operator!(const Y&) { return 11; }
inline int operator~(const Y&) { return 11; }
inline int operator++(const Y&) { return 11; }
inline int operator--(const Y&) { return 11; }
inline int operator++(const Y&, int) { return 11; }
inline int operator--(const Y&, int) { return 11; }
inline int operator,(const Y&, int) { return 11; }
inline int operator&(const Y&) { return 11; }
inline int operator+=(const Y&, int x) { return 11; }
inline int operator*=(const Y&, int x) { return 11; }
inline int operator-=(const Y&, int x) { return 11; }
inline int operator/=(const Y&, int x) { return 11; }

int
main ()
{
  Foo1 (0);
  Foo2 (0);
  Foo3 (0);
  Foo4 (0);
}
