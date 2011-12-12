// { dg-do compile }
// { dg-options "-fgnu-tm -O -std=c++0x -fdump-tree-tmmark -fdump-tree-tmlower" }

// Similar to noexcept-1.C but without an explicit (true) for noexcept.

struct TrueFalse
{
  static constexpr bool v() { return true; }
};

int global;

template<typename T> int foo()
{
  __transaction_atomic noexcept { global += 1; }
  return __transaction_atomic noexcept (global + 2)
         + __transaction_atomic noexcept (global + 3);
}

int f1()
{
  return foo<TrueFalse>();
}

int f3()
{
  __transaction_atomic noexcept { global += 4; }
  return __transaction_atomic noexcept (global + 5)
         + __transaction_atomic noexcept (global + 6);
}

/* { dg-final { scan-tree-dump-times "eh_must_not_throw" 6 "tmlower" } } */
/* { dg-final { scan-tree-dump-times "ITM_RU" 6 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmlower" } } */
