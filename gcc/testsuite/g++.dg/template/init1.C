// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/9457: ICE tsubst'ing initializers in templates.

template <typename> void foo (int count) {
  int i = {count};
}
template void foo<int> (int);
