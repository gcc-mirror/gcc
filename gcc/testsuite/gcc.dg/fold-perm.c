/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));

void fun (veci *f, veci *g, veci *h, veci *i)
{
  veci m = { 7, 7, 4, 6 };
  veci n = { 0, 1, 2, 3 };
  veci p = { 1, 1, 7, 6 };
  *i = __builtin_shuffle (*i,  p, m);
  *h = __builtin_shuffle (*h, *h, p);
  *g = __builtin_shuffle (*f, *g, m);
  *f = __builtin_shuffle (*f, *g, n);
}

/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 3, 3, 0, 2 }" "ccp1" } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 1, 1, 3, 2 }" "ccp1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 2 "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
