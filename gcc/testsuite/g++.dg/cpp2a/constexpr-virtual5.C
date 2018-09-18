// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct X1
{
  virtual int f1() const = 0;
  virtual int f2() const = 0;
  virtual int f3() const = 0;
};

struct X2: public X1
{
  int i2 = 42;
  constexpr virtual int f1() const { return 21; }
  constexpr virtual int f2() const { return 22; }
  constexpr virtual int f3() const { return 23; }
};

struct X3: public X2
{
  int i3 = 42;
  virtual int f1() const { return 31; }
  virtual int f2() const { return 32; }
  virtual int f3() const { return 33; }
};

struct X4: public X3
{
  int i4 = 42;
  constexpr virtual int f1() const { return 41; }
  constexpr virtual int f2() const { return 42; }
  constexpr virtual int f3() const { return 43; }
};

constexpr int (X1::*pf)() const = &X1::f2;

constexpr X2 x2;
static_assert(x2.f2() == 22);
static_assert((x2.*pf)() == 22);

constexpr X1 const& r2 = x2;
static_assert(r2.f2() == 22);
static_assert((r2.*pf)() == 22);

constexpr X1 const* p2 = &x2;
static_assert(p2->f2() == 22);
static_assert((p2->*pf)() == 22);

constexpr X4 x4;
static_assert(x4.f2() == 42);
static_assert((x4.*pf)() == 42);

constexpr X1 const& r4 = x4;
static_assert(r4.f2() == 42);
static_assert((r4.*pf)() == 42);

constexpr X1 const* p4 = &x4;
static_assert(p4->f2() == 42);
static_assert((p4->*pf)() == 42);
