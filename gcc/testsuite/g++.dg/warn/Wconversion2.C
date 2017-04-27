// { dg-options "-Wconversion-null" }
void foo(const char *); 
void bar() { foo(false); } // { dg-warning "pointer type for argument" "" { target { ! c++11 } } }
// { dg-error "cannot convert" "" { target c++11 } .-1 }
