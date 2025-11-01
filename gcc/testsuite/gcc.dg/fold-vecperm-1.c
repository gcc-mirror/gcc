/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop3" } */

typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));

typedef union {
  v4si s;
  v8hi h;
} int128;

int128 concat (int128 a, int128 b) {
  int128 x, y, res;
  v4si zero = { 0, 0, 0, 0 };
  v4si sel0 = { 3, 4, 5, 6 };
  v8hi sel1 = { 0, 1, 10, 11, 12, 13, 14, 15 };
  x.s = __builtin_shuffle (a.s, zero, sel0);
  y.s = __builtin_shuffle (zero, b.s, sel0);
  res.h = __builtin_shuffle (x.h, y.h, sel1);
  return res;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 1 "forwprop3" } } */
