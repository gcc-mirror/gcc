// PR c++/119574
// { dg-do compile { target c++20 } }

template <class F = decltype([] <auto G = [] {}> () {})>
void f(F op = {}) { op(); }

int main() { f(); }
