// { dg-do assemble  }

template <class T>
struct S1 {
};

template <>
struct S1<int> {};

struct S2 {
  friend class S1<int>;
};
