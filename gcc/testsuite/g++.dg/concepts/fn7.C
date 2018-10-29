// { dg-do link { target c++17 } }
// { dg-options "-fconcepts" }

// FIXME: What is this actually testing?

void f() requires true { }

int main() { }
