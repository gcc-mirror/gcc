// { dg-do compile }
// { dg-options "-fgnu-tm -O -std=c++0x -fdump-tree-tmmark" }

struct TrueFalse
{
};

int global;

template<typename T> int foo()
{
  return __transaction_atomic (global + 2)
         + __transaction_atomic (global + 3);
}

int f1()
{
  return foo<TrueFalse>();
}

/* { dg-final { scan-tree-dump-times "ITM_RU" 2 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
