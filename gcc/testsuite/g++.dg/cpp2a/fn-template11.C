// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

int nonconst ();

int foo ()
{
  return blah < // { dg-error "not declared" }
    nonconst (), nonconst (); // { dg-error "call to non-.constexpr. function" }
}
