/* Test C23 deprecated attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* Similar to tests from gnu23-attrs-1.c, but using the standard
   attribute instead of gnu::deprecated, and sometimes using
   __deprecated__ or a string-literal argument.  */

[[deprecated]] void f1 (void);

[[deprecated]] typedef int dep_int;

dep_int dv; /* { dg-warning "deprecated" } */

void
g (void)
{
  f1 (); /* { dg-warning "deprecated" } */
}

int
f2 (void)
{
  [[deprecated ("for this reason")]] int a = 1;
  return a; /* { dg-warning "for this reason" } */
}

int
f3 (void)
{
  int a [[__deprecated__]] = 1;
  return a; /* { dg-warning "deprecated" } */
}

struct s2 { [[__deprecated__("some other message")]] int a; int b [[deprecated]]; } x;

int
f4 (void)
{
  return x.a; /* { dg-warning "some other message" } */
}

int
f5 (void)
{
  return x.b; /* { dg-warning "deprecated" } */
}

enum e { E1 [[deprecated("third message")]] };

enum e
f6 (void)
{
  return E1; /* { dg-warning "third message" } */
}

int
f7 ([[deprecated]] int y)
{
  return y; /* { dg-warning "deprecated" } */
}

union [[__deprecated__]] u { int x; };

void
f8 (void)
{
  union u var; /* { dg-warning "deprecated" } */
}

enum [[deprecated("edep reason")]] edep { E2 };

void
f9 (void)
{
  enum edep var; /* { dg-warning "edep reason" } */
}

union u2 { [[__deprecated__]] int a; int b [[deprecated]]; } y;

int
f10 (void)
{
  return y.a; /* { dg-warning "deprecated" } */
}

int
f11 (void)
{
  return y.b; /* { dg-warning "deprecated" } */
}

struct [[deprecated]] s { int x; };

void
f12 (void)
{
  struct s var; /* { dg-warning "deprecated" } */
}
