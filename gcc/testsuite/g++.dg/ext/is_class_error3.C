struct A {};
void *p = __is_class (A);	// { dg-error "11:cannot convert" }
