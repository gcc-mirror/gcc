// PR c++/47068
// { dg-options -std=c++0x }

template <class T> struct broken {
  int member;
  typedef decltype(~ member) gcc_crashes_here;
};
