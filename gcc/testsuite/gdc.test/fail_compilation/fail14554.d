// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail14554.d(28): Error: `fail14554.issue14554_1.foo` called with argument types `(int)` matches both:
fail_compilation/fail14554.d(17):     `fail14554.issue14554_1.foo!bool.foo(int j)`
and:
fail_compilation/fail14554.d(18):     `fail14554.issue14554_1.foo!bool.foo(int j)`
fail_compilation/fail14554.d(29): Error: `fail14554.issue14554_2.foo` called with argument types `(int)` matches both:
fail_compilation/fail14554.d(22):     `fail14554.issue14554_2.foo!bool.foo(int j)`
and:
fail_compilation/fail14554.d(23):     `fail14554.issue14554_2.foo!bool.foo(int j)`
---
*/
struct issue14554_1 {
     void foo(T)(int j) {}
     static void foo(T)(int j) {}
}

struct issue14554_2 {
     static void foo(T)(int j) {}
     void foo(T)(int j) {}
}

void test14554()
{
     issue14554_1.foo!bool(1);    
     issue14554_2.foo!bool(1);    
}
