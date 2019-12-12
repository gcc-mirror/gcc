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

struct S
{
  int i, j;
  constexpr S() : i(x2.f()), j((x2.*pf)()) { }
};

static_assert(S().i == 2);
static_assert(S().j == 2);

constexpr X1 const& r2 = x2;

struct S2
{
  int i, j;
  constexpr S2() : i(r2.f()), j((r2.*pf)()) { }
};

static_assert(S2().i == 2);
static_assert(S2().j == 2);

constexpr X1 const* p2 = &x2;
struct S3
{
  int i, j;
  constexpr S3() : i(p2->f()), j((p2->*pf)()) { }
};

static_assert(S3().i == 2);
static_assert(S3().j == 2);

constexpr X4 x4;
struct S4
{
  int i, j;
  constexpr S4() : i(x4.f()), j((x4.*pf)()) { }
};

static_assert(S4().i == 4);
static_assert(S4().j == 4);

constexpr X1 const& r4 = x4;
struct S5
{
  int i, j;
  constexpr S5() : i(r4.f()), j((r4.*pf)()) { }
};

static_assert(S5().i == 4);
static_assert(S5().j == 4);

constexpr X1 const* p4 = &x4;
struct S6
{
  int i, j;
  constexpr S6() : i(p4->f()), j((p4->*pf)()) { }
};

static_assert(S6().i == 4);
static_assert(S6().j == 4);
