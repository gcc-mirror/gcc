// PR 81899 we tried to treat a bound-tpl-tpl-parm as-if a real record-type

template <template <typename> class FunctorData>
struct functor {
  friend class FunctorData<int>;
  void foo();
};

template <typename> struct data;

template<> void functor<data>::foo();
