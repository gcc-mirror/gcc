// { dg-do link { target c++14 } }
// { dg-options "-fconcepts" }

void f() requires true { }  // { dg-error "constraints on a non-templated function" }

int main() { }
