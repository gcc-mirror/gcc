// Test that new-expressions at file scope work properly.

struct A { static char* p; };

int i = 1;
char* A::p = new char[i];

void foo() {}
