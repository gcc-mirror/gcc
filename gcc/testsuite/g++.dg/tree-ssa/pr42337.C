// PR tree-optimize/42337
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-Wno-return-type" }

template<class _T1, class _T2> struct pair {
  _T2 second;
};
template<typename _Tp>
inline const _Tp& max(const _Tp& __a, const _Tp& __b) { }

template<typename _ForwardIterator, typename _Tp, typename _Compare> _ForwardIterator
lower_bound(_ForwardIterator __first, _ForwardIterator __last, const _Tp& __val, _Compare __comp) { }
template<class _CharT> struct char_traits {};

template<typename _Iterator, typename _Container> class __normal_iterator {
 public: typedef _Iterator iterator_type;
  __normal_iterator& operator++() {
  }
};
template<typename _IteratorL, typename _IteratorR, typename _Container>
inline bool operator!=(const __normal_iterator<_IteratorL, _Container>& __lhs, const __normal_iterator<_IteratorR, _Container>& __rhs) { }
template<typename _Tp> class new_allocator {
 public:
  typedef _Tp* pointer;
  typedef const _Tp* const_pointer;
};

template<typename _Tp>
class allocator: public new_allocator<_Tp> {
 public:
  template<typename _Tp1> struct rebind {
    typedef allocator<_Tp1> other;
  };
};

template<typename _Arg, typename _Result> struct unary_function { };
template<typename _Arg1, typename _Arg2, typename _Result> struct binary_function { };
template<typename _Tp> struct less : public binary_function<_Tp, _Tp, bool> { };
template<typename _Pair> struct _Select1st : public unary_function<_Pair, typename _Pair::first_type> { };
template<typename _Tp> struct _Rb_tree_iterator {
  typedef _Tp* pointer;
  pointer operator->() const {
  }
};
template<typename _Key, typename _Val, typename _KeyOfValue, typename _Compare, typename _Alloc = allocator<_Val> >
class _Rb_tree {
  typedef _Val value_type;
 public: typedef _Rb_tree_iterator<value_type> iterator;
};
template <typename _Key, typename _Tp, typename _Compare = less<_Key>, typename _Alloc = allocator<pair<const _Key, _Tp> > >
class map {
 public: typedef _Key key_type;
  typedef pair<const _Key, _Tp> value_type;
  typedef _Compare key_compare;
 private: typedef typename _Alloc::template rebind<value_type>::other _Pair_alloc_type;
  typedef _Rb_tree<key_type, value_type, _Select1st<value_type>, key_compare, _Pair_alloc_type> _Rep_type;
 public: typedef typename _Pair_alloc_type::pointer pointer;
  typedef typename _Rep_type::iterator iterator;
  iterator find(const key_type& __x) { }
};

template<typename _Tp, typename _Alloc> struct _Vector_base {
  typedef typename _Alloc::template rebind<_Tp>::other _Tp_alloc_type;
};
template<typename _Tp, typename _Alloc = allocator<_Tp> >
class vector : protected _Vector_base<_Tp, _Alloc> {
  typedef _Vector_base<_Tp, _Alloc> _Base;
  typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
 public:
  typedef typename _Tp_alloc_type::pointer pointer;
  typedef typename _Tp_alloc_type::const_pointer const_pointer;
  typedef __normal_iterator<pointer, vector> iterator;
  typedef __normal_iterator<const_pointer, vector> const_iterator;
  iterator begin() { }
  const_iterator begin() const { }
  const_iterator end() const { }
  unsigned long size() const { }
};

class SSC {
 public:
  SSC () {}
  SSC (const int& cs);
};
extern int flag;

struct TP {
   const int cl_;
   const vector<int> &its_;
   int max_s_;
 };

double foo(TP *p);
map<int, int> cs_;

template <typename T> class vector32 {
 public:
  typedef T& reference;
  typedef T* iterator;
  typedef const T* const_iterator;
  iterator begin() { return data_; }
  iterator end() { return data_ + size_; }
  long unsigned int size() const { return size_; }
  T* data_;
  unsigned size_;
};

struct SF : public pair<unsigned long long, double> { };

template<typename KEY, typename VALUE> class SFVT {
 private: typedef vector32<SF> Container;
  typedef typename Container::const_iterator CI;
  mutable Container v_;
  mutable bool sorted_;
  struct Cmp : public binary_function<SF, SF, bool> {
  };
  __attribute__((always_inline)) VALUE IS(const SFVT &sfv) const {
    if (sfv.v_.size() < v_.size()) {
      return sfv.IS(*this);
    }
    else {
      VALUE sum = 0.0;
      CI beg = sfv.v_.begin();
      CI end = sfv.v_.end();
      for (CI i = v_.begin();
           i != v_.end();
           ++i) { beg = lower_bound(beg, end, *i, Cmp()); if (beg == end) { return sum; } }
    }
  }
 public: explicit SFVT(const int capacity = 0) : sorted_(true) { }
  long unsigned int size() const { }
  __attribute__((always_inline)) VALUE DP(const SFVT &sfv) const {
    return IS(sfv);
  }
};
class SFV : public SFVT<unsigned long long, double> { };

class Edge;
extern int flag2;

double foo(TP *p) {
  int nbests_requested = max(p->max_s_, flag);
  map<int, int>::iterator it = cs_.find(p->cl_);
  int* c = &it->second;
  for (vector<int>::const_iterator iter = p->its_.begin();
       iter != p->its_.end();
       ++iter) {
  }
  vector<int*> fb;
  vector<double> w;
  int *hg = 0;
  if (flag2 == 10) {
    hg = &flag2;
  }
  int nr = 0;
  for (vector<int*>::iterator iter = fb.begin();
       (iter != fb.end() && nr < nbests_requested);
       ++iter) {
  }
  if (hg) {
    SFV s_weights;
    for (int i = 0;
         i < w.size();
         ++i) {
    }
    SFV uw;
    for (int i = 0, j = 0;
         i < uw.size() && j < s_weights.size();
         ) {
    }
    const double tc = uw.DP(s_weights);
  }
}
