// { dg-options "-std=c++1z" }

template<typename T>
  concept bool Tuple() { // { dg-error "multiple statements" }
    static_assert(T::value, "");
    return true;
  }

  void f(Tuple&);
