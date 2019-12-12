/*
TEST_OUTPUT:
---
fail_compilation/ice8255.d(10): Error: function ice8255.F!(G).F.f (ref G _param_0) is not callable using argument types (G)
fail_compilation/ice8255.d(10):        while evaluating pragma(msg, F().f(G()))
---
*/
struct G {}
struct F(T) { void f(ref T) {} }
pragma(msg, F!G().f(G.init));
