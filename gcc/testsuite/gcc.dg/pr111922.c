/* { dg-do compile } */
/* { dg-options "-Wno-old-style-definition -O2 -fno-tree-fre" } */

void f2 (void);
void f4 (int, int, int);
struct A { int a; };
struct B { struct A *b; int c; } v;

static int
f1 (x, y)
  struct C *x;
  struct A *y;
{
  (v.c = v.b->a) || (v.c = v.b->a);
  f2 ();
}

static void
f3 (int x, int y)
{
  int b = f1 (0, ~x);
  f4 (0, 0, v.c);
}

void
f5 (void)
{
  f3 (0, 0);
}
