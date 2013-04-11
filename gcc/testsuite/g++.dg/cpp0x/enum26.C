// PR c++/54216
// { dg-options "-std=c++11 -pedantic" }

enum {};            // { dg-warning "empty anonymous" }

enum class {};      // { dg-error "anonymous" }

enum class { x };   // { dg-error "anonymous" }
