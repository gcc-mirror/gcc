// PR c++/53783
// { dg-do compile { target c++11 } }

template <class T> void foo() { [] { [] {}; }; }
int main() { foo<void>(); }
