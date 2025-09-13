/* Test C2y register array element access: no warning in C23 mode with
   -Wno-c23-c2y-compat.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors -Wno-c23-c2y-compat" } */

extern void abort ();
extern void exit (int);

struct s { int a[10]; };

int
main ()
{
  int n = 10;
  register int a[1], b[20], c[n];
  register struct s v;
  a[0] = 0;
  for (int i = 0; i < n; i++)
    c[i] = i;
  for (int i = 0; i < 20; i++)
    b[i] = i;
  for (int i = 0; i < 10; i++)
    v.a[i] = i;
  for (int i = 0; i < n; i++)
    if (c[i] != i)
      abort ();
  for (int i = 0; i < 20; i++)
    if (b[i] != i)
      abort ();
  for (int i = 0; i < 10; i++)
    if (v.a[i] != i)
      abort ();
  if (a[0] != 0)
    abort ();
  if ((register int[2]) { 2, 3 }[n / 10] != 3)
    abort ();
  if ((register struct s) { 1, 2 }.a[n / 10] != 2)
    abort ();
  if (false)
    (void) a[12345];
  if (false)
    (void) b[23456];
  if (false)
    /* This index is not an integer constant expression, so the constraint
       against negative indices does not apply.  */
    (void) a[__INT_MAX__ + 2];
  /* { dg-warning "integer overflow in expression" "overflow" { target *-*-* } .-1 } */
  exit (0);
}
