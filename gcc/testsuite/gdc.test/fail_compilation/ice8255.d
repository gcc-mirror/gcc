/*
TEST_OUTPUT:
---
fail_compilation/ice8255.d(11): Error: function `ice8255.F!(G).F.f(ref G _param_0)` is not callable using argument types `(G)`
fail_compilation/ice8255.d(11):        cannot pass rvalue argument `G()` of type `G` to parameter `ref G _param_0`
fail_compilation/ice8255.d(11):        while evaluating `pragma(msg, F().f(G()))`
---
*/
struct G {}
struct F(T) { void f(ref T) {} }
pragma(msg, F!G().f(G.init));
