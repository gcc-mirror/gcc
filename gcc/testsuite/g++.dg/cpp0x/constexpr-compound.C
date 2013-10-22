// { dg-options "-std=c++11 -pedantic-errors" }

constexpr int f()
{
  {				// { dg-error "" }
    return 1;
  }
  { }				// { dg-error "" }
}
