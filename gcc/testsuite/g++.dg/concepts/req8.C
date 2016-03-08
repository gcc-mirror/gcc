// { dg-do compile }
// { dg-options "-std=c++1z -fconcepts" }

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
