// PR c++/54575
// { dg-do compile { target c++11 } }

template<typename _From, typename _To>
struct is_convertible { static const bool value = true; };

template<bool> struct enable_if       { };
template<>     struct enable_if<true> { typedef int type; };

template<typename _InIter>
using _RequireInputIter
= typename enable_if<is_convertible<_InIter,bool>::value>::type;

template<typename _Tp> struct X {
    template<typename _InputIterator,
         typename = _RequireInputIter<_InputIterator>>
      void insert(_InputIterator) {}
};

template<typename> void foo() {
  X<int> subdomain_indices;
  subdomain_indices.insert(0);
}
