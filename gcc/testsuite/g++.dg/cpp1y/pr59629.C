// { dg-do compile }
// { dg-options "-std=c++1y" }

// PR c++/59629

void foo(int i = []{ auto 0; }()); // { dg-error "expected|could not convert" }

