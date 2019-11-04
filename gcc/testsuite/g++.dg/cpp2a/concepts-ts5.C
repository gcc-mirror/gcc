// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

struct Base {
  template<typename T>
    static concept bool D() { return __is_same_as(T, int); } // { dg-error "a concept cannot be a member function" }

  template<typename T, typename U>
    static concept bool E() { return __is_same_as(T, U); } // { dg-error "a concept cannot be a member function" }
};
