// Tests non-unification of parms that don't use template parms.
// Build don't link:

enum kind {a, b};

class C { public: C () {} };

template<class P>
void f (P c, kind k) {}

template<class P>
void f (P c, P d, kind k) {}

template void f (C c, C, kind k);
