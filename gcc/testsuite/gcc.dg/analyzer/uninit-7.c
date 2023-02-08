typedef struct st
{
  char buf[16];
} st;

extern st foo (st);
extern st bar (st *);
extern char baz (st);

void test_1 (st a)
{
  st b, c, d, e;

  b = a;
  c = foo(a);
  d = bar(&a);
  c = foo(e); /* { dg-warning "use of uninitialized value 'e'" } */
}

void test_2 (st a)
{
  a = a;
}

st test_2a (void)
{
  st a;
  a = a; /* { dg-warning "use of uninitialized value 'a'" } */
  return a;
}

void test_3 (st a)
{
  a = foo (a);
}

st test_3a (void)
{
  st a;
  a = foo (a); /* { dg-warning "use of uninitialized value 'a'" } */
  return a;
}

void test_3b (st a, st b)
{
  a = foo (a);
  foo (b);
  a = foo (a);
  foo (b);
  a = foo (a);
  foo (b);
}

void test_4 (st a)
{
  a = bar (&a);
}

st test_4a (void)
{
  st a;
  a = bar (&a);
  return a;
}

void test_5 (st a)
{
  st b;
  a = bar (&a);
  b = b; /* { dg-warning "use of uninitialized value 'b'" } */
}

st test_6 (st a)
{
  st b;
  a = bar (&b);
  b = b;
  return b;
}

void test_6a (st a)
{
  st b;
  a = bar (&b);
  b = b;
}

st test_7 (st a)
{
  st b;
  b = bar (&a);
  return b;
}

void test_7a (st a)
{
  st b;
  b = bar (&a);
}

st test_8 (void)
{
  st b;
  b = bar (&b);
  return b;
}

void test_8a (void)
{
  st b;
  b = bar (&b);
}

char test_9 (st a)
{
  char c;
  c = baz (a);
  return c;
}

char test_10 (st a)
{
  char c;
  a = foo (a);
  c = baz (a);
  return c;
}
