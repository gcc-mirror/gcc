// { dg-do compile }
// { dg-additional-options "-fconcepts -fno-modules" }

export template<> a{};  // { dg-error "explicit spec" }
// { dg-warning "'export' not implemented" "" { target c++98_only } .-1 }
// { dg-warning "'export' is deprecated" "" { target { c++11 && c++17_down } } .-2 }
// { dg-warning "'export' is enabled with '-fmodules'" "" { target c++20 } .-3 }

// { dg-prune-output "'a' does not name a type" }
// { dg-prune-output "extra ';' outside of a function" }
