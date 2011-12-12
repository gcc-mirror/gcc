// { dg-do compile }
// { dg-options "-fgnu-tm -O -std=c++0x -fdump-tree-tmmark -fdump-tree-tmlower" }

struct TrueFalse
{
  static constexpr bool v() { return true; }
};

int global;

template<typename T> int foo()
{
  __transaction_atomic noexcept(T::v()) { global += 1; }
  return __transaction_atomic noexcept(T::v()) (global + 2);
}

int f1()
{
  return foo<TrueFalse>();
}

int f2()
{
  return __transaction_atomic noexcept(true) (global + 3)
         + __transaction_atomic noexcept(TrueFalse::v()) (global + 4);
}

int f3()
{
  __transaction_atomic noexcept(true) { global += 5; }
  __transaction_atomic noexcept(TrueFalse::v()) { global += 6; }
  return global;
}

/* { dg-final { scan-tree-dump-times "eh_must_not_throw" 6 "tmlower" } } */
/* { dg-final { scan-tree-dump-times "ITM_RU" 6 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmlower" } } */
