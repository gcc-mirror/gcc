/* Test C2y register array element access: C23 warning with -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic" } */

extern void abort ();
extern void exit (int);

struct s { int a[10]; };

int
main ()
{
  int n = 10;
  register int a[1], b[20], c[n];
  register struct s v;
  a[0] = 0; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  for (int i = 0; i < n; i++)
    c[i] = i; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  for (int i = 0; i < 20; i++)
    b[i] = i; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  for (int i = 0; i < 10; i++)
    v.a[i] = i; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  for (int i = 0; i < n; i++)
    if (c[i] != i) /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
      abort ();
  for (int i = 0; i < 20; i++)
    if (b[i] != i) /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
      abort ();
  for (int i = 0; i < 10; i++)
    if (v.a[i] != i) /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
      abort ();
  if (a[0] != 0) /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
    abort ();
  if ((register int[2]) { 2, 3 }[n / 10] != 3) /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
    abort ();
  if ((register struct s) { 1, 2 }.a[n / 10] != 2) /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
    abort ();
  if (false)
    (void) a[12345]; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  if (false)
    (void) b[23456]; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  if (false)
    /* This index is not an integer constant expression, so the constraint
       against negative indices does not apply.  */
    (void) a[__INT_MAX__ + 2]; /* { dg-warning "ISO C forbids subscripting 'register' array before C2Y" } */
  /* { dg-warning "integer overflow in expression" "overflow" { target *-*-* } .-1 } */
  exit (0);
}
