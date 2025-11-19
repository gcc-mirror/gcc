// PR c++/79474
// { dg-do compile { target c++14 } }
// { dg-options "-Wpedantic" }

struct Funject
{  
  operator auto() { return +[](bool b) {return b;}; }		   // { dg-warning "invalid use of 'auto' in conversion operator" }
  operator auto() { return +[](bool b, bool, bool) {return b;}; }  // { dg-error "cannot be overloaded" }
};								   // { dg-warning "invalid use of 'auto' in conversion operator" "" { target *-*-* } .-1 }

Funject fun;
auto bbb = fun(true);
auto bbbb = fun(true, false, true);  // { dg-error "no match" }
