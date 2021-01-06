// PR target/92038
// { dg-do compile { target int32 } }
// { dg-require-effective-target store_merge }
// { dg-options "-O2 -flifetime-dse=2 -fdump-tree-store-merging-details" }
// { dg-final { scan-tree-dump "New sequence of 2 stores to replace old one of 3 stores" "store-merging" } }

/* The alignment is necessary for store-merging to take place on
   strict-alignment targets.  */
struct __attribute__ ((__aligned__ (4))) T { char a[128]; };
struct S { S () : a () { a.a[12] = 0; a.a[13] = 1; a.a[14] = 0; a.a[15] = 6; } T a; };
void foo (S &);
void bar (void) { S s; foo (s); }
