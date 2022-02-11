/*
TEST_OUTPUT:
---
fail_compilation/fail8373.d(21): Error: `fail8373.fun1` called with argument types `(int)` matches both:
fail_compilation/fail8373.d(15):     `fail8373.fun1!().fun1!int.fun1(int)`
and:
fail_compilation/fail8373.d(16):     `fail8373.fun1!int.fun1(int)`
fail_compilation/fail8373.d(22): Error: `fail8373.fun2` called with argument types `(int)` matches both:
fail_compilation/fail8373.d(18):     `fail8373.fun2!int.fun2(int)`
and:
fail_compilation/fail8373.d(19):     `fail8373.fun2!().fun2!int.fun2(int)`
---
*/

template fun1(a...) { auto fun1(T...)(T args){ return 1; } }
                      auto fun1(T...)(T args){ return 2; }

                      auto fun2(T...)(T args){ return 2; }
template fun2(a...) { auto fun2(T...)(T args){ return 1; } }

enum x1 = fun1(0);
enum x2 = fun2(0);

