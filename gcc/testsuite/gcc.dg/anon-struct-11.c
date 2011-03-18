/* { dg-do compile } */

/* No special options--in particular, turn off the default
   -pedantic-errors option.  */
/* { dg-options "" } */

/* When not using -fplan9-extensions, we don't support automatic
   conversion of pointer types, and we don't support referring to a
   typedef name directly.  */

extern void exit (int);
extern void abort (void);

struct A { char a; };

struct B {
  char b;
  struct A;		/* { dg-warning "does not declare anything" } */
  char c;
};

void
f1 (struct A *p)	/* { dg-message "expected" } */
{
  p->a = 1;
}

void
test1 (void)
{
  struct B b;
  struct A *p;

  b.b = 2;
  b.c = 3;
  f1 (&b);		/* { dg-warning "incompatible pointer type" } */
  if (b.a != 1)		/* { dg-error "no member" } */
    abort ();
  if (b.b != 2 || b.c != 3)
    abort ();
  p = &b;		/* { dg-warning "incompatible pointer type" } */
  if (p->a != 1)
    abort ();
}

typedef struct { char d; } D;

struct E {
  char b;
  struct F { char f; };	/* { dg-warning "does not declare anything" } */
  char c;
  union {
    D;			/* { dg-warning "does not declare anything" } */
  };
  char e;
};

void
f2 (struct F *p)	/* { dg-message "expected" } */
{
  p->f = 6;
}

void
f3 (D *p)		/* { dg-message "expected" } */
{
  p->d = 4;
}

void
f4 (D d)
{
}

void
test2 (void)
{
  struct E e;
  struct F *pf;
  D *pd;
  D d;

  e.b = 2;
  e.c = 3;
  e.e = 5;
  f2 (&e);		/* { dg-warning "incompatible pointer type" } */
  f3 (&e);		/* { dg-warning "incompatible pointer type" } */
  if (e.d != 4)		/* { dg-error "no member" } */
    abort ();
  if (e.f != 6)		/* { dg-error "no member" } */
    abort ();
  if (e.b != 2 || e.c != 3 || e.e != 5)
    abort ();
  pf = &e;		/* { dg-warning "incompatible pointer type" } */
  if (pf->f != 6)
    abort ();
  pd = &e;		/* { dg-warning "incompatible pointer type" } */
  if (pd->d != 4)
    abort ();
  d = e.D;		/* { dg-error "no member" } */
  f3 (&e.D);		/* { dg-error "no member" } */
  f4 (e.D);		/* { dg-error "no member" } */
}

int
main ()
{
  test1 ();
  test2 ();
  exit (0);
}
