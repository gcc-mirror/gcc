// { dg-do compile { target c++14 } }

constexpr int p = 1;
constexpr __PTRDIFF_TYPE__ bar (int a)
{
  return ((__PTRDIFF_TYPE__) &p) << a; // { dg-error "is not a constant expression" }
}
constexpr __PTRDIFF_TYPE__ r = bar (2); // { dg-message "in .constexpr. expansion of" }
constexpr __PTRDIFF_TYPE__ s = bar (0); // { dg-error "conversion from pointer" }
