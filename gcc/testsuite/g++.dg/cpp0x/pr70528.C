// PR c++/70258
// { dg-do compile { target c++11 } }

template <class T>
struct H
{
  template <typename A = T, typename = decltype (A())>
  H ();
};

struct J {
  struct K {
    int First = 0;
  };
  H<K> FunctionMDInfo;
};
