/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
static int dummy = 0;
extern int foo __attribute__((__weak__, __alias__("dummy")));
int bar() { if (foo) return 1; return 0; }
/* { dg-final { scan-tree-dump-not "return 0" "optimized"} } */
