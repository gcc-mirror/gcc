/* Test C23 typeof and typeof_unqual.  Invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

struct s { int i : 2; } x;
union u { unsigned int j : 1; } y;

typeof (x.i) j; /* { dg-error "applied to a bit-field" } */
typeof_unqual (x.i) j2; /* { dg-error "applied to a bit-field" } */
typeof (y.j) j3; /* { dg-error "applied to a bit-field" } */
typeof_unqual (y.j) j4; /* { dg-error "applied to a bit-field" } */

static int ok (void);
static int also_ok (void);
static int not_defined (void); /* { dg-error "used but never defined" } */
static int also_not_defined (void); /* { dg-error "used but never defined" } */

void
f (void)
{
  typeof (ok ()) x = 2;
  typeof_unqual (also_ok ()) y = 2;
  int a[2];
  int (*p)[x] = &a;
  typeof (p + not_defined ()) q;
  typeof_unqual (p + also_not_defined ()) q2;
}
