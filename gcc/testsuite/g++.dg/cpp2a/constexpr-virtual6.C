// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct X1
{
  constexpr virtual X1 const *f() const { return this; }
};

struct Y
{
  int m = 0;
};

struct X2: public Y, public X1
{
  constexpr virtual X2 const *f() const { return this; }
};

constexpr X1 x1;
static_assert(x1.f() == &x1);

constexpr X2 x2;
constexpr X1 const& r2 = x2;
static_assert(r2.f() == &r2);
