// PR c++/79474
// { dg-do compile { target c++14 } }

struct Funject
{  
  operator auto() { return +[](bool b) {return b;}; }
  operator auto() { return +[](bool b, bool, bool) {return b;}; }  // { dg-error "cannot be overloaded" }
};

Funject fun;
auto bbb = fun(true);
auto bbbb = fun(true, false, true);  // { dg-error "no match" }
