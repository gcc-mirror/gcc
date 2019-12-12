// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct X1
{
  virtual int f() const = 0;
};

struct X2: public X1
{
  constexpr virtual int f() const { return 2; }
};

struct X3: public X2
{
  virtual int f() const { return 3; }
};

struct X4: public X3
{
  constexpr virtual int f() const { return 4; }
};

constexpr int (X1::*pf)() const = &X1::f;

constexpr X2 x2;
static_assert(x2.f() == 2);
static_assert((x2.*pf)() == 2);

constexpr X1 const& r2 = x2;
static_assert(r2.f() == 2);
static_assert((r2.*pf)() == 2);

constexpr X1 const* p2 = &x2;
static_assert(p2->f() == 2);
static_assert((p2->*pf)() == 2);

constexpr X4 x4;
static_assert(x4.f() == 4);
static_assert((x4.*pf)() == 4);

constexpr X1 const& r4 = x4;
static_assert(r4.f() == 4);
static_assert((r4.*pf)() == 4);

constexpr X1 const* p4 = &x4;
static_assert(p4->f() == 4);
static_assert((p4->*pf)() == 4);
