/* PR c/90677 */
/* { dg-do compile } */
/* { dg-options "-W -Wall" } */

extern void foo (int, int, const char *, ...)
  __attribute__ ((__format__ (__gcc_tdiag__, 3, 4)));
struct cgraph_node;
extern void bar (struct cgraph_node *);
