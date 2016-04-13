// PR c++/70209

struct V {
  typedef float F;
  template <typename S> void m_fn1(S);
};

template <typename> struct A {
  typedef V::F Ta __attribute__((__may_alias__));
  Ta *m_data;
  void m_fn2(V &);
};

template <>
void A<int>::m_fn2(V &p) {
  p.m_fn1(m_data);
}
