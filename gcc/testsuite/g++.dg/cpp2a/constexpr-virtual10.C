// P1064R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct X
{
  constexpr virtual int f() const { return 1; };
};

struct Y : public X
{
  constexpr virtual int f() const { return 2; };
};

constexpr X a[2][1][3];
constexpr Y b[3][12];
static_assert (a[1][0][1].f() == 1);
static_assert (b[2][11].f() == 2);
