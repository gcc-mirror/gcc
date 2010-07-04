/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void
f1 (void)
{
  int a;	/* { dg-warning "set but not used" } */
  int b;
  int c;
  c = 1;
  a = b = c;
}

void
f2 (int x)
{
  int a;	/* { dg-warning "set but not used" } */
  int b;
  int c;	/* { dg-warning "set but not used" } */
  c = (a = x, b = x);
}

int
f3 (int x)
{
  int a;
  return a = x;
}

int
f4 (int x)
{
  int a;
  a = x;
  return a;
}

void
f5 (int x)
{
  int a[2];	/* { dg-warning "set but not used" } */
  int b;
  int *c, d[2];
  c = d;
  b = x;
  a[b] = 1;
  c[b] = 1;
}

int
f6 (int x)
{
  int a[2];
  int b;
  b = x;
  a[b] = 1;
  return a[b];
}

void
f7 (int x, int *p)
{
  int *a[2];
  a[x] = p;
  a[x][x] = x;
}

struct S { int i; };

void
f8 (void)
{
  struct S s;	/* { dg-warning "set but not used" } */
  s.i = 6;
}

int
f9 (void)
{
  struct S s;
  s.i = 6;
  return s.i;
}

struct S
f10 (void)
{
  struct S s;
  s.i = 6;
  return s;
}

extern int foo11 (int *);

void
f11 (void)
{
  int a[2];
  foo11 (a);
}

void
f12 (void)
{
  int a;
  a = 1;
  a;	/* { dg-warning "no effect" } */
}

void
f13 (void (*x) (void))
{
  void (*a) (void);
  a = x;
  a ();
}

void
f14 (void (*x) (void))
{
  void (*a) (void);	/* { dg-warning "set but not used" } */
  a = x;
}

extern void foo15 (int *);

void
f15 (void)
{
  int a[10];
  int *b = a + 2;
  foo15 (b);
}

extern void foo16 (int **);

void
f16 (void)
{
  int a[10];
  int *b[] = { a, a + 2 };
  foo16 (b);
}

void
f17 (int x)
{
  long a;	/* { dg-warning "set but not used" } */
  int b;
  a = b = x;
}

void
f18 (int x)
{
  int a;	/* { dg-warning "set but not used" } */
  int b;
  a = (char) (b = x);
}

int
f19 (int x, int y, int z)
{
  int a;
  int b;
  a = x;
  b = y;
  return z ? a : b;
}

int *
f20 (int x)
{
  static int a[] = { 3, 4, 5, 6 };
  static int b[] = { 4, 5, 6, 7 };
  static int c[] = { 5, 6, 7, 8 };	/* { dg-warning "set but not used" } */
  c[1] = 1;
  return x ? a : b;
}
