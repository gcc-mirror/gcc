// this file is part of testcase instantiate5.C

template <class T> void foo() { }
inline int bar() { foo<void>(); return 1; }
static int i = bar();
