// Test that we complain about redeclaration with different visibility

struct __attribute((visibility("hidden"))) B;
struct __attribute((visibility("default"))) B;	// { dg-warning "visibility" }

__attribute ((visibility ("hidden"))) void f();	// { dg-warning "previous" }
__attribute ((visibility ("default"))) void f(); // { dg-warning "visibility" }
