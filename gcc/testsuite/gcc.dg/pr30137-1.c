/* { dg-do compile } */
/* { dg-options "-w -fdump-tree-gimple" } */

/* Things that should not be folded.  */

struct { long base; int tail; void * volatile ptr; } *s;
int foo3 (void) { return s == &s; }
int foo5 (void) { return s->ptr == s->ptr; }

struct { union { int i; short s } u; } x;
int foo6 (void) { return x.u.i == x.u.s; }

void **p;
int foo8 (void) { return p == &p; }
int foo9 (void) { return *p == p; }
int foo10 (void) { return *p == &p; }
int foo11 (void) { return p != &p; }
int foo12 (void) { return *p != p; }
int foo13 (void) { return *p != &p; }

/* { dg-final { scan-tree-dump-not "= 0;" "gimple" } } */
/* { dg-final { scan-tree-dump-not "= 1;" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
