// PR c++/53356
// { dg-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-not "= 0" "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }

class A {};

struct B {
    operator const A &() const;
};

A* cause_ICE() {
    return new A(B());
}
