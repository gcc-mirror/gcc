/* PR c/34146 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

struct A
{
  int f1, f2, f3;
};

struct B
{
  struct A g1, g2;
};

struct C
{
  struct B h1, h2;
};

typedef union
{
  struct C c;
  char s[4];
  long int a;
} T;

void
foo (void)
{
  T t = (T) { { { { 0, 0, 0 }, { 0, 0, 0 } }, { { 0, 0, 0 }, { 0, 0, 0 } } } };
  test (&t);
}

void
bar (void)
{
  T t = (T) { { { { 0, 0, 0 }, (struct A) { 0, 0, 0 } },
	      (struct B) { (struct A) { 0, 0, 0 }, { 0, 0, 0 } } } };
  test (&t);
}

void
baz (void)
{
  T t = (T) { { { { 0, 0, 0 }, (struct A) { 1, 1, 1 } },
	      (struct B) { (struct A) { 0, 0, 0 }, { 1, 1, 1 } } } };
  test (&t);
}

/* { dg-final { scan-tree-dump-not "t = D" "gimple"} } */
/* { dg-final { scan-tree-dump-not "t\.c\.h\[12\] = D" "gimple"} } */
/* { dg-final { scan-tree-dump-not "\.g\[12\] = D" "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
