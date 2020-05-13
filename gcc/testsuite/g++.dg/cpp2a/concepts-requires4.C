// { dg-do compile { target c++20 } }

// Test associated type requirements

// req8.C

template<typename T>
concept Has_member_type = requires { typename T::type; };

template<typename T>
concept Concept = true && Has_member_type<T>;

template<typename T>
  requires Concept<T>
void foo(T t) { }
