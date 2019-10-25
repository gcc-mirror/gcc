// { dg-do link { target c++14 } }
// { dg-options "-fconcepts" }

void f() requires true { }

int main() { }
