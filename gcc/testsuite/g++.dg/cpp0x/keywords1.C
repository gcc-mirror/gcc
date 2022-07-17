// PR c++/106111
// { dg-do compile { target c++98_only } }
// { dg-options "-Wc++11-compat" }

int alignof; // { dg-warning "is a keyword in C\\\+\\\+11" }
int alignas; // { dg-warning "is a keyword in C\\\+\\\+11" }
int constexpr; // { dg-warning "is a keyword in C\\\+\\\+11" }
int decltype; // { dg-warning "is a keyword in C\\\+\\\+11" }
int noexcept; // { dg-warning "is a keyword in C\\\+\\\+11" }
int nullptr; // { dg-warning "is a keyword in C\\\+\\\+11" }
int static_assert; // { dg-warning "is a keyword in C\\\+\\\+11" }
int thread_local; // { dg-warning "is a keyword in C\\\+\\\+11" }
int _Alignas;
int _Alignof;
int _Thread_local;
