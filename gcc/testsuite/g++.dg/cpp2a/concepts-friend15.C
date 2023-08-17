// PR c++/109751
// { dg-do compile { target c++20 } }

template<typename _Tp> concept cmpeq
  = requires(_Tp __t, _Tp __u) { { __u != __t } ; };

template<typename D>
struct iterator_interface
{
  friend constexpr bool operator>=(D lhs, D rhs)
    requires cmpeq<D> { return true; }
};

template<typename T>
struct iterator : iterator_interface<iterator<T>>
{
    bool operator==(iterator) const;
    iterator &operator++();
    iterator &operator++(int);
};

static_assert(cmpeq<iterator<int>>);
