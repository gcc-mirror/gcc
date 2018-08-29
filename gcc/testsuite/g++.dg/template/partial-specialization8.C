// PR c++/55639

template <int number>
struct SomeClass
{
  SomeClass() { }

  template <typename E, int number2>
  struct Fun {
    static void
    fun() { }
  };
};

template <int number>
template <typename E>
struct SomeClass<number>::template Fun<E, 0> { // { dg-error "template" }
  static void fun() { }
};
