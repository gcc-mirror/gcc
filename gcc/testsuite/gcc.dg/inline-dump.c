/* Verify that -fopt-info can output correct inline info.  */
/* { dg-do compile } */
/* { dg-options "-Wall -fopt-info-inline=stderr -O2 -fno-early-inlining" } */
static inline int leaf() {
  int i, ret = 0;
  for (i = 0; i < 10; i++)
    ret += i;
  return ret;
}
static inline int foo(void) { return leaf(); } /* { dg-message "note: leaf .*inlined into bar .*via inline instance foo.*\n" } */
int bar(void) { return foo(); }
