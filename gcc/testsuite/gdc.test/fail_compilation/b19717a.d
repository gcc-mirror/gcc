// REQUIRED_ARGS: -de
/* TEST_OUTPUT:
---
fail_compilation/b19717a.d(14): Error: forward reference to template `a`
fail_compilation/b19717a.d(14): Error: forward reference to template `a`
fail_compilation/b19717a.d(14): Error: none of the overloads of `a` are callable using argument types `()`
fail_compilation/b19717a.d(13):        Candidates are: `b19717a.a(int b)`
fail_compilation/b19717a.d(14):                        `b19717a.a(int b = a)`
---
*/
module b19717a;

auto a(int b) {}
auto a(int b = a) {}
