// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

void
foo ()
{
  [[fallthrough]];			// { dg-error "invalid use of attribute 'fallthrough'" }
lab:;
  goto lab;
}
