// PR c++/77591
// { dg-do compile { target c++14 } }
// { dg-options "-O0 -Wreturn-local-addr" }

class A { };

decltype(auto)
foo ()
{
  A c;
  return (c);	   // { dg-warning "reference to local variable 'c' returned" }
}

decltype(auto)
bar ()
{
  A c;
  return 1==1 ? c : c; // { dg-warning "reference to local variable 'c' returned" }
}
