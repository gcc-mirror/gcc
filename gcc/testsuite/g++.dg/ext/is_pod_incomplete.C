// PR c++/32158
template<typename T>
  struct A
  {
    A() { }
  };

int t[__is_pod(A<int>)?-1:1];
