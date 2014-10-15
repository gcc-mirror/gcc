/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void link_error (void);

struct S
{
  int w, x, y, z;
};

struct T
{
  int r;
  struct S s;
};

struct S bar (struct S x, struct S *y)
{
  y->w = 4;
  return *y;
}

void
foo (int a, struct T b)
{
  struct S x;
  struct S *c = &x;
  if (a)
    c = &b.s;
  b.s.w = 3;
  /* This call should be marked as clobbering 'x' and 'b'.  */
  *c = bar (*c, c);
  if (b.s.w == 3)
    abort ();
}

float Y;

struct S bar1 (struct S x, struct S y)
{
  Y = 4;
  return x;
}

void
foo1 (int a, struct T b)
{
  struct S x;
  struct S *c = &x;
  float z, *k = &z;
  if (a)
    c = &b.s;
  b.s.w = 3;
  /* This call should NOT be marked as clobbering 'x' and 'b'.  */
  x = bar1 (*c, *c);
  if (b.s.w != 3)
    link_error ();
}

int main ()
{
  struct T b;
  foo (3, b);
  foo1 (3, b);
  return 0;
}
