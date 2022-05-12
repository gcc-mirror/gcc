// PR c++/102300

template<typename T>
struct holder
{
  template<typename F> struct fn {};

  struct t1 : fn<T> {};                      // pass
  struct t2 : holder<T >::fn<T> {};          // fail
  struct t3 : holder<T >::template fn<T> {}; // fail
  struct t4 : holder<T*>::template fn<T> {}; // pass
};
