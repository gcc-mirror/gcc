// PR c++/27423
// { dg-do compile }

void foo(void = 0);    // { dg-error "incomplete type|invalid use" }
void bar() { foo(); }
