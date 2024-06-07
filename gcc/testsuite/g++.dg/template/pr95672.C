// PR c++/95672
// { dg-do compile { target c++14 } }
struct g_class : decltype  (auto) ... {  }; // { dg-error "contains no parameter packs" }
