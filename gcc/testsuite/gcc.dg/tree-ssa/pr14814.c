/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop2" } */


struct YY {
  double e[3];  };

static inline double *y(struct YY* this_1) { return &this_1->e[1]; }

struct XX {
  struct YY v;
 };

static inline struct YY direction (const struct  XX* this_1) { return this_1->v;}

int foo(const struct XX* r) {
  struct YY t = direction(r);
  if (*y(&t) < 0.000001) return 0;
  return 1;
}

/* { dg-final { scan-tree-dump-times "&" 0 "forwprop2" } } */
/* { dg-final { cleanup-tree-dump "forwprop2" } } */


