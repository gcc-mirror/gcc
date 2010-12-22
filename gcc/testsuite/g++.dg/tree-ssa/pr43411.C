/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
class P { public: virtual int val() { return 123; } };
class Psub : public P { };

extern int sink1, sink2;

void test() {
    Psub p;
    P &pRef = p;
    sink1 = p.val();
    sink2 = pRef.val();
}


inline int v(P &p) { return p.val(); }

void testInlineP() {
    P p;
    sink1 = v(p);
}

void testInlinePsub() {
    Psub p;
    sink1 = v(p);
}

// { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "optimized" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "optimized" } }
