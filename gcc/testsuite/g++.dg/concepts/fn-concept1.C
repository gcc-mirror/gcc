// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool Tuple() { // { dg-error "multiple statements" }
    static_assert(T::value, "");
    return true;
  }

  void f(Tuple&);
