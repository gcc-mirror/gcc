/* { dg-do compile } */
/* { dg-options "-Og -fdump-tree-gimple" } */

struct s {
  short a : 3;
  short b : 3;
  char  c;
};

extern void g(struct s *);

void f() {
  struct s x = { 0, 0, 1 };
  g (&x);
}

/* { dg-final { scan-tree-dump-times "= {};" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-not "= 0;" "gimple" } } */
