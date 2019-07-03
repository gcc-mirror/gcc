// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

struct A { explicit (!__builtin_is_constant_evaluated ()) A (int); };	// { dg-warning "'explicit\\(bool\\)' only available with" "" { target c++17_down } }
struct B { explicit (!std::is_constant_evaluated ()) B (int); };	// { dg-warning "'explicit\\(bool\\)' only available with" "" { target c++17_down } }
struct C { explicit (__builtin_is_constant_evaluated ()) C (int); };	// { dg-warning "'explicit\\(bool\\)' only available with" "" { target c++17_down } }
struct D { explicit (std::is_constant_evaluated ()) D (int); };		// { dg-warning "'explicit\\(bool\\)' only available with" "" { target c++17_down } }
A a = 1;
B b = 2;
C c = 3;	// { dg-error "conversion from 'int' to non-scalar type 'C' requested" }
D d = 4;	// { dg-error "conversion from 'int' to non-scalar type 'D' requested" }
