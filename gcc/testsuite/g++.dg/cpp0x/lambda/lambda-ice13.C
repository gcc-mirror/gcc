// PR c++/61088
// { dg-do compile { target c++11 } }

void f()
{
  typedef void (*X) ();
  X x[] = { [x](){} };  // { dg-error "incomplete type" }
}

void g()
{
  typedef void (X) ();
  X x[] = { [x](){} };  // { dg-error "5:declaration of .x. as array of functions" }
  // { dg-error "not declared" "" { target *-*-* } .-1 }
}
