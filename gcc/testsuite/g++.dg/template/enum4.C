// PR c++/18020

template <typename> struct bar {
  enum {
    e1 = 1,
    e2 = ~e1
  };
};
template struct bar<int>;
