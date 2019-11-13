// { dg-require-effective-target c++11 }

template <bool> struct A;
class Vector {
  template <typename> struct TypeIsGCThing {
    template <typename T, typename A<T ::value>::Type> using Vector = Vector;
    struct B;
    template <typename> class ContainerIter {
      using Action = B;
      using ActionVector = Vector<Action, 0>;
      ContainerIter<ActionVector> a;
    };
  };
};
