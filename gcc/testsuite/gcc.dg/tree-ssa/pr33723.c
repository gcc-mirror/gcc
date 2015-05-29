/* PR tree-optimization/33723 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

typedef union
{
  struct
  {
    int f1, f2, f3, f4, f5, f6, f7, f8;
    long int f9, f10;
    int f11;
  } f;
  char s[4];
  long int a;
} T;

void test (T *);

void
foo1 (void)
{
  T t;
  t = (T) { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };
  test (&t);
}

void
bar1 (void)
{
  T t = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };
  test (&t);
}

void
baz1 (void)
{
  T t;
  t = (const T) { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };
  test (&t);
}

void
foo2 (void)
{
  T t;
  t = (T) { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 } };
  test (&t);
}

void
bar2 (void)
{
  T t = { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 } };
  test (&t);
}

void
baz2 (void)
{
  T t;
  t = (const T) { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 } };
  test (&t);
}

void
baz3 (void)
{
  T t;
  t = (const T) (T) { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 } };
  test (&t);
}

/* { dg-final { scan-tree-dump-not "t = D" "gimple"} } */
