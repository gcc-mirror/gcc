// PR c++/33101

typedef void v;
typedef v (*pf)(v);  // { dg-error "invalid use of typedef-name" }
