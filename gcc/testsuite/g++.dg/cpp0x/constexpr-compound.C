// { dg-do compile { target c++11 } }

constexpr int f()
{
  {				// { dg-error "" }
    return 1;
  }
  { }				// { dg-error "" }
}
