// pr C++/86374
// bogus lookup error
template<typename LIST>
struct list {
  static const int index = 1;
  template <typename> struct addWithChecking {};
};

template<typename container, int ix = container::index>
struct find {
  static const int result = 0;
};

template <class LIST>
template<class O>
struct list<LIST>::addWithChecking<O*>
{
  static const int xres =
    find<list<LIST> >::result; // bogus error about index here.
};
