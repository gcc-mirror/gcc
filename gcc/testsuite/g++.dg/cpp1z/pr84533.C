// { dg-do compile { target c++17 } }

enum E { e, e };  // { dg-error "redefinition" }
