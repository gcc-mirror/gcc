// PR c++/54021
// { dg-do compile { target c++11 } }

extern int nonconst_func(int);
constexpr int identity(int x) { return x; }
constexpr int zero() { return identity(0); }
constexpr int one() { return identity(1); }

// These are the same.  Only the latter is accepted, though.
constexpr int rejected_const_4(int x)
{ return __builtin_constant_p(x) ? 4 : nonconst_func(x); }
constexpr int accepted_const_4(int x)
{ return identity(__builtin_constant_p(x)) ? 4 : nonconst_func(x); }

// This is rejected.  I would like it to work.
constexpr int four = accepted_const_4(1);
