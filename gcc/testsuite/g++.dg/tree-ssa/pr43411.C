/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-release_ssa" } */
class P { public: virtual int function_to_inline() { return 123; } };
class Psub : public P { };

extern int sink1, sink2;

void test() {
    Psub p;
    P &pRef = p;
    sink1 = p.function_to_inline();
    sink2 = pRef.function_to_inline();
}


inline int v(P &p) { return p.function_to_inline(); }

void testInlineP() {
    P p;
    sink1 = v(p);
}

void testInlinePsub() {
    Psub p;
    sink1 = v(p);
}

// { dg-final { scan-tree-dump-not "function_to_inline" "release_ssa" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "release_ssa" } }
