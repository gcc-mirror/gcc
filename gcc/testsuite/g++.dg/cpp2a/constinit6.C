// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++17_down } }
// { dg-options "-Wc++20-compat" }

int constinit; // { dg-warning "identifier .constinit. is a keyword" }
