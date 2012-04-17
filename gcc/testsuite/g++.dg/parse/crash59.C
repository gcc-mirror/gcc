// PR c++/53003

struct A{ void a{} return b  // { dg-error "function definition|expected" }
