// PR c++/79588
// { dg-do compile }
// { dg-options "-Wrestrict" }

void foo (char *__restrict, char *__restrict = __null);

template <int N>
void
bar (char **p)
{
  foo (p[0], p[0]);	// { dg-warning "to restrict-qualified parameter aliases with" }
  foo (p[0], p[N]);	// { dg-warning "to restrict-qualified parameter aliases with" }
  foo (p[0]);
}

template <int N>
void
bar2 (char **p)
{
  foo (p[0], p[0]);	// { dg-warning "to restrict-qualified parameter aliases with" }
  foo (p[0], p[N]);	// { dg-bogus "to restrict-qualified parameter aliases with" }
  foo (p[0]);
}

void
baz (char **p)
{
  bar<0> (p);
  bar2<1> (p);
}
