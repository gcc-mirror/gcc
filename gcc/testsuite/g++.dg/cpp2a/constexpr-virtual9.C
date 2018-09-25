// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct X1
{
  virtual int f() const = 0;
  virtual int f(int) const = 0;
  virtual int f(int, int) const = 0;
};

struct X2: public X1
{
  constexpr virtual int f() const { return 2; }
  constexpr virtual int f(int) const { return 12; }
  constexpr virtual int f(int, int) const { return 22; }
};

struct X3: public X2
{
  virtual int f() const { return 3; }
  virtual int f(int) const { return 13; }
  virtual int f(int, int) const { return 23; }
};

struct X4: public X3
{
  constexpr virtual int f() const { return 4; }
  constexpr virtual int f(int) const { return 14; }
  constexpr virtual int f(int, int) const { return 24; }
};

constexpr int (X1::*pf)() const = &X1::f;
constexpr int (X1::*pf1)(int) const = &X1::f;
constexpr int (X1::*pf2)(int, int) const = &X1::f;

constexpr X2 x2;
static_assert(x2.f() == 2);
static_assert((x2.*pf)() == 2);
static_assert(x2.f(1) == 12);
static_assert((x2.*pf1)(1) == 12);
static_assert(x2.f(1, 2) == 22);
static_assert((x2.*pf2)(1, 2) == 22);

constexpr X1 const& r2 = x2;
static_assert(r2.f() == 2);
static_assert((r2.*pf)() == 2);
static_assert(r2.f(1) == 12);
static_assert((r2.*pf1)(1) == 12);
static_assert(r2.f(1, 2) == 22);
static_assert((r2.*pf2)(1, 2) == 22);

constexpr X1 const* p2 = &x2;
static_assert(p2->f() == 2);
static_assert((p2->*pf)() == 2);
static_assert(p2->f(1) == 12);
static_assert((p2->*pf1)(1) == 12);
static_assert(p2->f(1, 2) == 22);
static_assert((p2->*pf2)(1, 2) == 22);

constexpr X4 x4;
static_assert(x4.f() == 4);
static_assert((x4.*pf)() == 4);
static_assert(x4.f(1) == 14);
static_assert((x4.*pf1)(1) == 14);
static_assert(x4.f(1, 2) == 24);
static_assert((x4.*pf2)(1, 2) == 24);

constexpr X1 const& r4 = x4;
static_assert(r4.f() == 4);
static_assert((r4.*pf)() == 4);
static_assert(r4.f(1) == 14);
static_assert((r4.*pf1)(1) == 14);
static_assert(r4.f(1, 2) == 24);
static_assert((r4.*pf2)(1, 2) == 24);

constexpr X1 const* p4 = &x4;
static_assert(p4->f() == 4);
static_assert((p4->*pf)() == 4);
static_assert(p4->f(1) == 14);
static_assert((p4->*pf1)(1) == 14);
static_assert(p4->f(1, 2) == 24);
static_assert((p4->*pf2)(1, 2) == 24);
