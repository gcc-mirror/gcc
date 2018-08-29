// { dg-options "-std=c++17" }

enum E { e, e };  // { dg-error "redefinition" }
