// Before P1787 (C++20), only hidden friends are included in ADL.
// After P1787, all friends are included.

// { dg-additional-options -fmodules }

import M;

int main()
{
  N::A a;
  fn(a);			// { dg-error "not declared" "" { target c++17_down } }
}
