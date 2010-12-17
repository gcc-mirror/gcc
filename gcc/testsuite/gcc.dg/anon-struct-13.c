/* { dg-do compile } */
/* { dg-options "-fplan9-extensions" } */

/* Test for ambiguity when using the Plan 9 extensions.  */

struct A {
  char a;		/* { dg-error "duplicate member" } */
};

struct B
{
  struct A;
  struct A;
};

char
f1 (struct B *p)
{
  return p->a;		/* { dg-error "no member" } */
}

void
f2 (struct A *p)	/* { dg-message "expected" } */
{
}

void
f3 (struct B *p)
{
  f2 (p);		/* { dg-warning "incompatible pointer type" } */
}

struct C
{
  char c;		/* { dg-error "duplicate member" } */
};

struct D
{
  struct C;
};

struct E
{
  struct C;
  struct D;
};

char
f4 (struct E *p)
{
  return p->c;		/* { dg-error "no member" } */
}

void
f6 (struct C *p)	/* { dg-message "expected" } */
{
}

void
f7 (struct E *p)
{
  f6 (p);		/* { dg-warning "incompatible pointer type" } */
}

struct A
f8 (struct B *p)
{
  return p->A;		/* { dg-error "no member" } */
}

struct C
f9 (struct E *p)
{
  return p->C;		/* { dg-error "no member" } */
}
