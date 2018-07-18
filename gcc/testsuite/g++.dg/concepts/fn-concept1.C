// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool Tuple() { // { dg-error "multiple statements" }
    static_assert(T::value, "");
    return true;
  }

  void f(Tuple&);
