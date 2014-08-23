// PR c++/59629
// { dg-do compile { target c++14 } }

void foo(int i = []{ auto 0; }()); // { dg-error "expected|could not convert" }
