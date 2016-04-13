// { dg-do compile }
// { dg-options "-O2 -std=c++11 -fno-pic" }
// { dg-require-effective-target fpic }

template<typename _Tp, _Tp __v>
struct integral_constant
{
  static constexpr _Tp value = __v;
  typedef _Tp value_type;
  typedef integral_constant<_Tp, __v> type;
  constexpr operator value_type() const { return value; }
};

typedef integral_constant<bool, true> true_type;
extern void xxx (true_type c);

void
yyy (void)
{
  true_type y;
  xxx (y);
}

// { dg-final { scan-assembler "jmp\[\t \]+\[^\$\]*?_Z3xxx17integral_constantIbLb1EE" { target i?86-*-* x86_64-*-* } } }
