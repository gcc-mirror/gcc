// PR c++/77591
// { dg-do compile { target c++14 } }
// { dg-options "-O0 -Wreturn-local-addr" }

class A { };

decltype(auto)
foo ()
{
  A c;			// { dg-warning "reference to local variable 'c' returned" }
  return (c);
}

decltype(auto)
bar ()
{
  A c;			// { dg-warning "reference to local variable 'c' returned" }
  return 1==1 ? c : c;
}
