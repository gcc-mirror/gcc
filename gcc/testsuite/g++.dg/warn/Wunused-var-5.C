// { dg-do compile }
// { dg-options "-Wunused" }

template <int N>
void
f1 (void)
{
  int a;	// { dg-warning "set but not used" }
  int b;
  int c;
  c = 1;
  a = b = c;
}

template <int N>
void
f2 (int x)
{
  int a;	// { dg-warning "set but not used" }
  int b;
  int c;	// { dg-warning "set but not used" }
  c = (a = x, b = x);
}

template <int N>
int
f3 (int x)
{
  int a;
  return a = x;
}

template <int N>
int
f4 (int x)
{
  int a;
  a = x;
  return a;
}

template <int N>
void
f5 (int x)
{
  int a[2];	// { dg-warning "set but not used" }
  int b;
  int *c, d[2];
  c = d;
  b = x;
  a[b] = 1;
  c[b] = 1;
}

template <int N>
int
f6 (int x)
{
  int a[2];
  int b;
  b = x;
  a[b] = 1;
  return a[b];
}

template <int N>
void
f7 (int x, int *p)
{
  int *a[2];
  a[x] = p;
  a[x][x] = x;
}

struct S { int i; };

template <int N>
void
f8 (void)
{
  struct S s;	// { dg-warning "set but not used" }
  s.i = 6;
}

template <int N>
int
f9 (void)
{
  struct S s;
  s.i = 6;
  return s.i;
}

template <int N>
struct S
f10 (void)
{
  struct S s;
  s.i = 6;
  return s;
}

extern int foo11 (int *);

template <int N>
void
f11 (void)
{
  int a[2];
  foo11 (a);
}

template <int N>
void
f12 (void)
{
  int a;
  a = 1;
  a;	// { dg-warning "statement has no effect" }
}

template <int N>
void
f13 (void (*x) (void))
{
  void (*a) (void);
  a = x;
  a ();
}

template <int N>
void
f14 (void (*x) (void))
{
  void (*a) (void);	// { dg-warning "set but not used" }
  a = x;
}

extern void foo15 (int *);

template <int N>
void
f15 (void)
{
  int a[10];
  int *b = a + 2;
  foo15 (b);
}

extern void foo16 (int **);

template <int N>
void
f16 (void)
{
  int a[10];
  int *b[] = { a, a + 2 };
  foo16 (b);
}

template <int N>
void
f17 (int x)
{
  long a;	// { dg-warning "set but not used" }
  int b;
  a = b = x;
}

template <int N>
void
f18 (int x)
{
  int a;	// { dg-warning "set but not used" }
  int b;
  a = (char) (b = x);
}

template <int N>
int
f19 (int x, int y, int z)
{
  int a;
  int b;
  a = x;
  b = y;
  return z ? a : b;
}

template <int N>
int *
f20 (int x)
{
  static int a[] = { 3, 4, 5, 6 };
  static int b[] = { 4, 5, 6, 7 };
  static int c[] = { 5, 6, 7, 8 };	// { dg-warning "set but not used" }
  c[1] = 1;
  return x ? a : b;
}

S s;

void
test ()
{
  int i = 0;
  f1<0> ();
  f2<0> (0);
  (void) f3<0> (0);
  (void) f4<0> (0);
  f5<0> (0);
  (void) f6<0> (0);
  f7<0> (0, &i);
  f8<0> ();
  (void) f9<0> ();
  s = f10<0> ();
  f11<0> ();
  f12<0> ();
  f13<0> (f1<0>);
  f14<0> (f1<0>);
  f15<0> ();
  f16<0> ();
  f17<0> (0);
  f18<0> (0);
  (void) f19<0> (0, 0, 0);
  (void) f20<0> (0);
}
