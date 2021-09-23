/* PR middle-end/98871 - Cannot silence -Wmaybe-uninitialized at declaration
   site
   { dg-do compile }
   { dg-options "-O1 -Wall" } */

struct A
{
  int x;
};

// Verify that suppression works at every inlining level.

static int f0 (int *x)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"

  return ++*x;

#pragma GCC diagnostic pop
}

static int f1 (int *p, int n)
{
  struct A a;
  for (int i = 0; i < n; ++i) {
    if (p[i] > 1) {
      a = (struct A){p[i]};
    }
  }
	
  return f0 (&a.x);
}

int f2 (void)
{
  int a[] = { 1, 2, 3, 4 };
  return f1 (a, 4);
}


static int g0 (int *x)
{
  return ++*x;
}

static int g1 (int *p, int n)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"

  struct A a;
  for (int i = 0; i < n; ++i) {
    if (p[i] > 1) {
      a = (struct A){p[i]};
    }
  }
	
  return g0 (&a.x);

#pragma GCC diagnostic pop
}

int g2 (void)
{
  int a[] = { 1, 2, 3, 4, 5 };
  return g1 (a, 5);
}


static int h0 (int *x)
{
  return ++*x;
}

static int h1 (int *p, int n)
{
  struct A a;
  for (int i = 0; i < n; ++i) {
    if (p[i] > 1) {
      a = (struct A){p[i]};
    }
  }
	
  return h0 (&a.x);
}

int h2 (void)
{
  int a[] = { 1, 2, 3, 4, 5, 6 };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"

  return h1 (a, 6);

#pragma GCC diagnostic pop
}
