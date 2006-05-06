// PR c++/27422
// { dg-do compile }

void foo(void i);      // { dg-error "incomplete type|invalid use" }
void bar() { foo(0); }
