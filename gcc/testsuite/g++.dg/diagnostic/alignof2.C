void f();
int i = __alignof(f); // { dg-error "19:ISO C\\+\\+ forbids applying .__alignof." }
