// PR c++/45307
// { dg-options "-fdump-tree-gimple -fdump-tree-optimized -O" }

struct fallible_t { };
const fallible_t fallible = fallible_t();

void t(void)
{
}

// { dg-final { scan-tree-dump-not "fallible" "gimple" } }
// Whole constructor should be optimized away.
// { dg-final { scan-tree-dump-not "int" "optimized" } }
// { dg-final { cleanup-tree-dump "gimple" } }
// { dg-final { cleanup-tree-dump "optimized" } }
