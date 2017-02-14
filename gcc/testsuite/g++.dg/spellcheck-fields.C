/* { dg-do compile } */

struct foo
{
  int foo;
  int bar;
  int baz;
};

int test (struct foo *ptr)
{
  return ptr->m_bar; /* { dg-error "'struct foo' has no member named 'm_bar'; did you mean 'bar'?" } */
}

int test2 (void)
{
  struct foo instance = {0, 0, 0};
  return instance.m_bar; /* { dg-error "'struct foo' has no member named 'm_bar'; did you mean 'bar'?" } */
}

struct s {
    struct j { int aa; } kk;
    int ab;
};

void test3 (struct s x)
{
  x.ac;  /* { dg-error "'struct s' has no member named 'ac'; did you mean 'ab'?" } */
}

int test4 (struct foo *ptr)
{
  return sizeof (ptr->foa); /* { dg-error "'struct foo' has no member named 'foa'; did you mean 'foo'?" } */
}

/* Verify that we don't offer nonsensical suggestions.  */

int test5 (struct foo *ptr)
{
  return ptr->this_is_unlike_any_of_the_fields;   /* { dg-bogus "did you mean" } */
  /* { dg-error "has no member named" "" { target *-*-* } 40 } */
}

union u
{
  int color;
  int shape;
};

int test6 (union u *ptr)
{
  return ptr->colour; /* { dg-error "'union u' has no member named 'colour'; did you mean 'color'?" } */
}

struct has_anon
{
  struct { int color; } s;
};

int test7 (struct has_anon *ptr)
{
  return ptr->s.colour; /* { dg-error "'struct has_anon::<unnamed>' has no member named 'colour'; did you mean 'color'?" } */
}

int test8 (foo &ref)
{
  return ref.m_bar; /* { dg-error "'struct foo' has no member named 'm_bar'; did you mean 'bar'?" } */
}

struct bar : public foo
{
  int fizz;
  typedef int my_type;
};

int test9 (bar *ptr)
{
  return ptr->fuzz; /* { dg-error "'struct bar' has no member named 'fuzz'; did you mean 'fizz'?" } */
}

int test10 (bar *ptr)
{
  return ptr->m_foo; /* { dg-error "'struct bar' has no member named 'm_foo'; did you mean 'foo'?" } */
}

int test11 (bar *ptr)
{
  return ptr->mytype; /* { dg-error "'struct bar' has no member named 'mytype'; did you mean 'my_type'?" } */
}
