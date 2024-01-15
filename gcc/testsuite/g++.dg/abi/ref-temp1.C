// From ABI document
// { dg-do compile { target c++14 } }
// { dg-skip-if "No .weak" { { hppa*-*-hpux* } && { ! lp64 } } }

struct A { const int (&x)[3]; };
struct B { const A (&x)[2]; };
template <typename T> B &&b = { { { { 1, 2, 3 } }, { { 4, 5, 6 } } } };
B &temp = b<void>;

// { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZGR1bIvE_" } }
// { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZGR1bIvE0_" } }
// { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZGR1bIvE1_" } }
// { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZGR1bIvE2_" } }

// { dg-final { scan-assembler "_ZGR1bIvE_:\n\[^\n]+_ZGR1bIvE0_" } }
// { dg-final { scan-assembler "_ZGR1bIvE0_:\n\[^\n]+_ZGR1bIvE1_" } }
// { dg-final { scan-assembler "_ZGR1bIvE1_:\n\[^\n]+\[ \t\]1" } }
// { dg-final { scan-assembler "_ZGR1bIvE2_:\n\[^\n]+\[ \t\]4" } }
