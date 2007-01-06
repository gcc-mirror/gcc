/* { dg-do compile } */
/* { dg-options "--param ggc-min-heapsize=0 --param ggc-min-expand=0" } */
/* This used to ICE because we forgot to update the statement after folding
   and the eh info. */
/* PR tree-opt/30385 */

static inline void g(int t)
{
 int a;
 while (a < f())
  ;
}
void h(int t)
{
 g(t);
}
