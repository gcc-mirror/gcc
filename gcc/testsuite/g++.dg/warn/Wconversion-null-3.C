// PR c++/48420
// { dg-do compile { target { ! c++11 } } }

void foo(int* p);

void bar() {
  const bool kDebugMode = false;
  foo(kDebugMode);   // { dg-warning "converting 'false'" }
}
