// PR c++/28387
// { dg-do compile }

enum __attribute__((unused)) E;  // { dg-error "without previous declaration" }
