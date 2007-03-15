// { dg-options "-Wconversion" }

void foo(const char *); 
void bar() { foo(false); } // { dg-warning "pointer type for argument" }
