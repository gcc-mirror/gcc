// PR c++/60250
// { dg-options "-std=c++1y -pedantic-errors" }

template<typename> void foo()
{
  typedef int T[ ([](){ return 1; }()) ]; // { dg-error "runtime bound" }
}
