/* PR middle-end/31096 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define signval_pos(name, op) \
int name (int a, int b) \
{ return a * 4 op b * 4; }

signval_pos(peq, ==) signval_pos(pne, !=) signval_pos(plt, <)
signval_pos(pgt, >)  signval_pos(pge, >=) signval_pos(ple, <=)

#define signval_neg(name, op) \
int name (int a, int b) \
{ return a * -23 op b * -23; }

signval_neg(neq, ==) signval_neg(nne, !=) signval_neg(nlt, <)
signval_neg(ngt, >)  signval_neg(nge, >=) signval_neg(nle, <=)

#define vec_pos(name, op) \
int name (int a[10], int b[10]) \
{ return a[3] * 4 op b[8] * 4; }

vec_pos(vpeq, ==) vec_pos(vpne, !=) vec_pos(vplt, <)
vec_pos(vpgt, >)  vec_pos(vpge, >=) vec_pos(vple, <=)

#define vec_neg(name, op) \
int name (int a[10], int b[10]) \
{ return a[3] * -23 op b[8] * -23; }

vec_neg(vneq, ==) vec_neg(vnne, !=) vec_neg(vnlt, <)
vec_neg(vngt, >)  vec_neg(vnge, >=) vec_neg(vnle, <=)

/* { dg-final { scan-tree-dump-not "\\(D\\) \\* 4" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\(D\\) \\* -23" "optimized" } } */
/* { dg-final { scan-tree-dump-times "_1 = b_2\\(D\\)" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "_1 = MEM\\\[\\(int \\*\\)b" 4 "optimized" } } */
