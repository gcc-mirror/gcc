struct __attribute((deprecated ("foo"))) A { }; // { dg-message "declared" }
void f(const A&) { }		// { dg-warning "deprecated: foo" }
