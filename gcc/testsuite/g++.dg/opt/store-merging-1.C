// PR target/92038
// { dg-do compile { target int32 } }
// { dg-require-effective-target store_merge }
// { dg-options "-O2 -flifetime-dse=2 -fdump-tree-store-merging-details" }
// { dg-final { scan-tree-dump "New sequence of \[12] stores to replace old one of 2 stores" "store-merging" } }

struct S { S () : a (0), b (0) {} int a; char b; char c[3]; };
void foo (struct S);
void bar (void) { struct S s; foo (s); }
