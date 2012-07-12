// PR c++/53039
// { dg-do compile { target c++11 } }

template <class, class>
struct is_convertible
{
  static const bool value = true;
};

template<bool, class T>
struct enable_if
{
  typedef T type;
};

template <bool...>
struct Xs
{
  static const bool value = true;
};

template<typename... BTs>
  class BType
    {
      template <typename... BUs,
        typename enable_if<
               Xs<is_convertible<BUs, BTs>::value...>::value,
               bool>::type = false>
        void fooX(BUs&&...);
    };

template <typename... ATs>
  struct AType
    {
      template <typename... AUs,
    typename enable_if<
               Xs<is_convertible<AUs, ATs>::value...>::value,
               bool>::type = false>
        void foo(AUs&&...);
    };

int main()
{
  AType<int, int> t;
  t.foo(1, 1);
}
