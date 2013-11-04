// PR c++/47068
// { dg-options -std=c++11 }

template <class T> struct broken {
  int member;
  typedef decltype(~ member) gcc_crashes_here;
};
