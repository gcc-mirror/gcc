// PR c++/119574
// A version of lambda-targ13.C with extra template parameters.
// { dg-do compile { target c++20 } }

template <int = 0, class F = decltype([] <int = 1, auto G = [] {}> () {})>
void f(F op = {}) { op(); }

int main() { f(); }
