// PR c++/67685
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

void f(auto i) {requires {i;};}

int main() {f(0);}
