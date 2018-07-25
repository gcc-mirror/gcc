// PR c++/85634

namespace bsl {
  template <class T> void frob (const T *);
}

using namespace bsl;

template<class VALUE> void frob (const VALUE &);

template <typename T>
struct TPL {
  friend void frob <T> (const T &);
};

TPL<int> x;
