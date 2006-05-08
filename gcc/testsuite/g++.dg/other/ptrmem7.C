// PR c++/27447
// { dg-do compile }

void (A::* p)();  // { dg-error "declared|token" }
