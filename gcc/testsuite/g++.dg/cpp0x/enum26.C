// PR c++/54216
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

enum {};            // { dg-warning "empty unnamed" }

enum class {};      // { dg-error "unnamed" }

enum class { x };   // { dg-error "unnamed" }
