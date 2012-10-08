// PR c++/28387
// { dg-do compile { target c++11 } }

enum [[gnu::unused]] E;  // { dg-error "without previous declaration" }
