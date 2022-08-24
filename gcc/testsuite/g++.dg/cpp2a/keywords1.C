// PR c++/106111
// { dg-do compile { target c++17_down } }
// { dg-options "-Wc++20-compat -Wc++11-compat -Wc++14-compat -Wc++17-compat" }

int constinit; // { dg-warning "is a keyword in C\\\+\\\+20" }
int consteval; // { dg-warning "is a keyword in C\\\+\\\+20" }
int requires; // { dg-warning "is a keyword in C\\\+\\\+20" }
int concept; // { dg-warning "is a keyword in C\\\+\\\+20" }
int co_await; // { dg-warning "is a keyword in C\\\+\\\+20" }
int co_yield; // { dg-warning "is a keyword in C\\\+\\\+20" }
int co_return; // { dg-warning "is a keyword in C\\\+\\\+20" }
int char8_t; // { dg-warning "is a keyword in C\\\+\\\+20" }
