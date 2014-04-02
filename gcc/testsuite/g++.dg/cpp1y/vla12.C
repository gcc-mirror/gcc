// PR c++/60250
// { dg-do compile { target c++1y } }

template<typename> void foo()
{
  typedef int T[ ([](){ return 1; }()) ]; // { dg-error "runtime bound" }
}
