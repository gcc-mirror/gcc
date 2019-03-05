// { dg-do compile { target c++11 } }
// { dg-additional-options "-fconcepts" }

using A = auto;  // { dg-error "11:.auto. not allowed in alias declaration" }

using A1 = const auto;  // { dg-error "18:.auto. not allowed in alias declaration" }

using A2 = volatile auto;  // { dg-error "21:.auto. not allowed in alias declaration" }

using A3 = const volatile auto;  // { dg-error "27:.auto. not allowed in alias declaration" }

typedef auto B;  // { dg-error "9:typedef declared .auto." }

typedef const auto B1;  // { dg-error "15:typedef declared .auto." }

typedef volatile auto B2;  // { dg-error "18:typedef declared .auto." }

typedef const volatile auto B3;  // { dg-error "24:typedef declared .auto." }
