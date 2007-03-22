// { dg-do assemble  }
// Origin: Leon Bottou <leonb@research.att.com>

class AA { protected:
  template <class T> struct BB { T x; BB(const T &x) : x(x) { } };
  template <class T> struct CC : public BB<T> { CC(const T &x) : BB<T>(x) { }
};
};
