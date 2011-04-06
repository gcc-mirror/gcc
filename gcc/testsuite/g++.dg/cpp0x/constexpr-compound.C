// { dg-options "-std=c++0x -pedantic-errors" }

constexpr int f()
{
  {				// { dg-error "" }
    return 1;
  }
  { }				// { dg-error "" }
}
