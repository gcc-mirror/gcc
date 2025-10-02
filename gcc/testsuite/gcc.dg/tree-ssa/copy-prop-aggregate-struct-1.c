/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details -fdump-tree-optimized" } */
/* PR tree-optimization/121841 */

struct U2 {
   int i[1024];
};

struct U2 g_284[1] = {{0UL}};
struct U2 g_283[1];

/* g_284[0] and g_283[0] are known not to
   overlap so a copy prop can happen. */
void func_1() {
  struct U2 removeme;
  removeme = g_284[0];
  g_283[0] =  removeme;
}

/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "removeme " "optimized" } } */
