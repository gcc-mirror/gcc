// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Check that type requirements are normalized correctly.

template<typename T>
  concept bool Has_member_type() {
    return requires() { typename T::type; };
  }

template<typename T>
  concept bool Concept() {
    return true && Has_member_type<T>();
  }

template<Concept T>
  void foo( T t  ) { }
