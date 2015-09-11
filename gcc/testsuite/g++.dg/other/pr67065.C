// PR c++/67065

int main;  // { dg-error "cannot declare" }

void foo() { int main; }
