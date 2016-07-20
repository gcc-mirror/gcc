// PR c++/50060
// { dg-do compile { target c++11 } }

extern "C" double frexp (double, int *);

struct S
{
  constexpr S (double a) : y {}, x (frexp (a, &y)) {}	// { dg-error "is not a constant expression" "S" { target { ! c++14 } } }
  double x;
  int y;
};

struct T
{
  constexpr T (double a) : y {}, x ((y = 1, 0.8125)) {}	// { dg-error "is not a constant-expression" "T" { target { ! c++14 } } }
  double x;
  int y;
};

static_assert (S (6.5).x == 0.8125, "");	// { dg-error "non-constant condition for static assertion|in constexpr expansion" "" { target { ! c++14 } } }
static_assert (T (6.5).x == 0.8125, "");	// { dg-error "non-constant condition for static assertion|called in a constant expression" "" { target { ! c++14 } } }
