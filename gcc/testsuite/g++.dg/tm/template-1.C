// { dg-do compile }
// { dg-options "-fgnu-tm -O -fdump-tree-tmmark" }

struct TrueFalse
{
  static bool v() { return true; }
};

int global;

template<typename T> int foo()
{
  __transaction_atomic { global += 2; }
  return __transaction_atomic (global + 1);
}

template<typename T> int bar() __transaction_atomic
{
  return global + 3;
}

template<typename T> void bar2() __transaction_atomic
{
  global += 4;
}

int f1()
{
  bar2<TrueFalse>();
  return foo<TrueFalse>() + bar<TrueFalse>();
}

/* 4 transactions overall, two of them write to global:  */
/* { dg-final { scan-tree-dump-times "ITM_RU4\\s*\\(&global" 4 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "ITM_WU4\\s*\\(&global" 2 "tmmark" } } */
