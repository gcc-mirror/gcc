// PR c++/83947
// { dg-do compile { target c++14 } }

auto f ();
template < int > auto g (f);	// { dg-error "before deduction" }
auto h = g < 0 > ();
