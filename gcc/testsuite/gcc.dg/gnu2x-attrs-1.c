/* Test C2x attribute syntax.  Test GNU attributes appertain to
   appropriate constructs.  */
/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-std=gnu2x" } */

void f (void) {};

[[gnu::alias("f")]] void g (void); /* { dg-error "only weak" "" { target *-*-darwin* } } */

void [[gnu::alias("f")]] h (void); /* { dg-warning "does not apply to types" } */

struct [[gnu::packed]] s { int a; char b; };
_Static_assert (sizeof (struct s) == (sizeof (int) + sizeof (char)));

int
f2 (void)
{
  [[gnu::deprecated]] int a = 1;
  return a; /* { dg-warning "deprecated" } */
}

int
f3 (void)
{
  int a [[gnu::deprecated]] = 1;
  return a; /* { dg-warning "deprecated" } */
}

struct s2 { [[gnu::deprecated]] int a; int b [[gnu::deprecated]]; } x;

int
f4 (void)
{
  return x.a; /* { dg-warning "deprecated" } */
}

int
f5 (void)
{
  return x.b; /* { dg-warning "deprecated" } */
}

enum e { E1 [[gnu::deprecated]] };

enum e
f6 (void)
{
  return E1; /* { dg-warning "deprecated" } */
}

int
f7 ([[gnu::deprecated]] int y)
{
  return y; /* { dg-warning "deprecated" } */
}

union [[gnu::deprecated]] u { int x; };

void
f8 (void)
{
  union u var; /* { dg-warning "deprecated" } */
}

enum [[gnu::deprecated]] edep { E2 };

void
f9 (void)
{
  enum edep var; /* { dg-warning "deprecated" } */
}
