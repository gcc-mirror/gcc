// PR c++/95672
// { dg-do compile { target c++14 } }
struct g_class : decltype  (auto) ... {  }; // { dg-error "invalid use of pack expansion" }
