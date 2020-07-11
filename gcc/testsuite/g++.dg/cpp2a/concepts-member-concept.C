// { dg-do compile { target c++20 } }

// FIXME: Diagnostics should be better.

struct Base {
  template<typename T>
  static concept D = __is_same_as(T, int); // { dg-error "static data member" }

  template<typename T, typename U>
  static concept E = __is_same_as(T, U); // { dg-error "static data member" }

  template<typename T>
  concept C1 = __is_same_as(T, int); // { dg-error "not in namespace scope" }
};

union U {
  template<typename T>
  concept C = true; // // { dg-error "not in namespace scope" }
};

// { dg-excess-errors "deprecated" }
