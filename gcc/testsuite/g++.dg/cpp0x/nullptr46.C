// PR c++/101443
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

decltype(nullptr) foo ();

bool
bar ()
{
  return foo () > nullptr // { dg-error "ordered comparison" }
    || foo () < nullptr; // { dg-error "ordered comparison" }
}
