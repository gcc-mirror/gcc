// { dg-do compile }
// { dg-options "" }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/9459: typeof in return type of template function

void foo (int) {}
void foo (double) {}

template <typename C>
typeof(foo(1))
bar () { return foo(1); }
