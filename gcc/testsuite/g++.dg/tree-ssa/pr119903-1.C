// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fnon-call-exceptions -ftrapping-math -fdump-tree-optimized-eh" }

// PR tree-optimization/119903
// match and simplify would cause the internal throwable fp comparison
// to become only external throwable and lose the landing pad.

int f() noexcept;
int g() noexcept;

int m(double a)
{
  try {
    if (a < 1.0)
      return f();
    return g();
  }catch(...)
  {
        return -1;
  }
}

// Make sure There is a landing pad for the non-call exception from the comparison.
// { dg-final { scan-tree-dump "LP " "optimized" } }
