// { dg-do compile }
// { dg-options "-std=c++98" }
enum class E1 { e1 };  // { dg-warning "scoped enums" }
enum E2 : char { e2 }; // { dg-warning "scoped enums" }
