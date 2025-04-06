// PR c++/118626
// { dg-do compile { target c++20 } }

template<long> struct _Nth_type { using type = _Nth_type; };

template<class _Up>
struct variant {
  template<class _Tp> static constexpr long __accepted_index = 0;
  template<long _Np> using __to_type = typename _Nth_type<_Np>::type;
  template<class _Tp, int = sizeof(_Tp)> using __accepted_type = __to_type<__accepted_index<_Tp>>;
  template<class = __accepted_type<_Up>> variant(_Up);
};

template<class _Tp>
struct Node { Node(_Tp); };

template<class R> using Tree = variant<Node<R>>;
using type = decltype(Tree{Node{42}});
using type = Tree<int>;
