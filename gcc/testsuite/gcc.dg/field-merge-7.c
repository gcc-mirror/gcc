/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that the third compare won't be combined with the first one.  */

struct s {
  char a, b;
  int p;
};

struct s a = { 0, 0, 0 };
struct s b = { 0, 0, 0 };

int f () {
  return (a.a != b.a || (a.p != b.p && a.b != b.b));
}

int g () {
  return (a.a == b.a && (a.p == b.p || a.b == b.b));
}

/* { dg-final { scan-tree-dump-not "optimizing" "ifcombine" } } */
/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "ifcombine" } } */
