// PR c++/54216
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

enum {};            // { dg-warning "empty anonymous" }

enum class {};      // { dg-error "anonymous" }

enum class { x };   // { dg-error "anonymous" }
