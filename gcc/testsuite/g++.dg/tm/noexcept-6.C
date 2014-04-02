// { dg-do compile { target c++11 } }
// { dg-options "-fno-exceptions -fgnu-tm -O -fdump-tree-tmlower" }

struct TrueFalse
{
  static constexpr bool v() { return true; }
};

int global;

template<typename T> int foo()
{
  return __transaction_atomic noexcept(T::v()) (global + 1);
}

int f1()
{
  return foo<TrueFalse>();
}

/* { dg-final { scan-tree-dump-times "eh_must_not_throw" 0 "tmlower" } } */
/* { dg-final { scan-tree-dump-times "__transaction_atomic" 1 "tmlower" } } */
/* { dg-final { cleanup-tree-dump "tmlower" } } */
