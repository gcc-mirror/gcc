// PR c++/92106 - ICE with structured bindings and -Wreturn-local-addr. 
// { dg-do compile { target c++17 } }

template <typename> struct B;
template <typename _Tp> struct B<_Tp *> { typedef _Tp& reference; };
struct C {
  template <typename _Up> using rebind = _Up *;
};
template <typename _Iterator, typename> class D {
public:
  typename B<_Iterator>::reference operator*();
  void operator++();
};

template <typename _Iterator, typename _Container>
bool operator!=(D<_Iterator, _Container>, D<_Iterator, _Container>);
template <typename _Tp> class F {
public:
  typedef _Tp value_type;
};

template <typename _Alloc> struct G {
  template <typename _Tp> struct H { using type = C::rebind<_Tp>; };
  using const_pointer = typename H<typename _Alloc::value_type>::type;
};
template <typename _Tp, typename _Alloc = F<_Tp>> class I {
  typedef D<typename G<_Alloc>::const_pointer, int> const_iterator;

public:
  const_iterator begin();
  const_iterator end();
};

struct A {
  struct J {
    int name;
    int value;
  };
  I<J> members;
  template <typename Key> const int *find(Key) {
    for (const auto &[name, value] : members)
      // See <https://gcc.gnu.org/ml/gcc-patches/2019-10/msg01107.html>
      // for why we don't warn here.
      return &value; // { dg-bogus "address of local variable" }
    return nullptr;
  }
};
int main() {
  A a;
  a.find("");
}
