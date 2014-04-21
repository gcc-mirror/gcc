// { dg-do compile { target c++11 } }

struct S;

typedef int (*F [[gnu::warn_unused_result]]) (int);

typedef int (*F2 [[gnu::warn_unused_result]]) (int);

typedef int (S::*F3 [[gnu::warn_unused_result]]) (int);

typedef int [[gnu::warn_unused_result]] (*F5) (int); // { dg-warning "ignored" }
