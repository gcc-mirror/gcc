/* Regression test for paste appearing at the beginning of a set of
   actual arguments.  Original bug exposed by Linux kernel.  Problem
   reported by Jakub Jelinek <jakub@redhat.com>.  */

/* { dg-do compile } */

extern int foo(int x);

#define bar(x) foo(x)
#define baz(x) bar(##x)

int quux(int y) { return baz(y); }  /* { dg-error "valid preprocessing" } */
