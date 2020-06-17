// PR tree-optimization/93210
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-not "static_member\.d" "optimized" } }

union U { struct { unsigned int a, b; } c; unsigned long long d; };

inline
bool operator == (U const &x, U const &y) noexcept
{
  return x.d == y.d;
};

struct S
{
  static constexpr U static_member = { { 13, 42 } };
  bool foo (U const &y) const noexcept;
  bool bar (U const &y) const noexcept;
};

#if __cpp_inline_variables < 201606L
constexpr U S::static_member;
#endif

#if __SIZEOF_INT__ * 2 == __SIZEOF_LONG_LONG__
bool
S::foo (U const &y) const noexcept
{
  return static_member == y;
}

bool
S::bar (U const &y) const noexcept
{
  return U (static_member) == y;
}
#endif
