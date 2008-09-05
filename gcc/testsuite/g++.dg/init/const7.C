// { dg-do compile }
// { dg-options "-fdump-tree-gimple" }

struct s { int x, y; };
short offsets[1] = {
  ((char*) &(((struct s*)16)->y) - (char *)16),
};

// This ensures that we get a dump whether or not the bug is present.
void fn() { }

// { dg-final { scan-tree-dump-not "initialization"  "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }
