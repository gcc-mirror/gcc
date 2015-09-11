// { dg-options "-Wall" }

#define FOO "foo"
const char *p = "bar"FOO;	// { dg-warning "macro" }
