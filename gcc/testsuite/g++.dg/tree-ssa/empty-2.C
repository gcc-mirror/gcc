// PR c++/45307
// { dg-options -fdump-tree-gimple }

struct fallible_t { };
const fallible_t fallible = fallible_t();

// { dg-final { scan-tree-dump-not "fallible" "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }
