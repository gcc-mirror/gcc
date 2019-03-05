// PR c++/70507 - integer overflow builtins not constant expressions
// The constexpr-arith-overflow.C testcase covers this for C++14 and later.
// { dg-do compile }
// { dg-options "-std=c++11" }

#define Assert(expr) static_assert ((expr), #expr)

template <class T>
constexpr T add (T x, T y, T z = T ())
{
  return __builtin_add_overflow (x, y, &z) ? 0 : z;	// { dg-error "is not a constant expression" }
}

template <class T>
constexpr T sub (T x, T y, T z = T ())
{
  return __builtin_sub_overflow (x, y, &z) ? 0 : z;	// { dg-error "is not a constant expression" }
}

template <class T>
constexpr T mul (T x, T y, T z = T ())
{
  return __builtin_mul_overflow (x, y, &z) ? 0 : z;	// { dg-error "is not a constant expression" }
}

Assert (0 == add<int>(0, 0));	// { dg-error "non-constant condition for static assertion" }
Assert (0 == sub<int>(0, 0));	// { dg-error "non-constant condition for static assertion" }
Assert (0 == mul<int>(0, 0));	// { dg-error "non-constant condition for static assertion" }
// { dg-message "expansion of" "" { target *-*-* } .-3 }
// { dg-message "expansion of" "" { target *-*-* } .-3 }
// { dg-message "expansion of" "" { target *-*-* } .-3 }
