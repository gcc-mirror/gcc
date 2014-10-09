// { dg-do compile { target c++11 } }

constexpr int f()
{
  {				// { dg-error "compound-statement" "" { target { c++11_only } } }
    return 1;
  }
  { }				// { dg-error "compound-statement" "" { target { c++11_only } } }
}
