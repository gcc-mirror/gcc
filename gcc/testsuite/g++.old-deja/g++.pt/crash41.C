// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <int> struct S1{};

struct S2 { int i; };

template <class T>
void f(S2 s2) {
  S1<s2.i> s1;
}
