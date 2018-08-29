// PR c++/84801
// { dg-do compile { target c++14 } }

int v;
int main() { [](auto... c) { v = c; }(1); } // { dg-error "not expanded" }
