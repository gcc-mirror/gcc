// PR 13764: We were inserting an extra body block in all functions, but
// it's only really necessary for [cd]tors.
// { dg-options "-fdump-tree-gimple" }

void bar (void)
{
  int a;
}

// { dg-final { scan-tree-dump-times "\{" 1 "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }
