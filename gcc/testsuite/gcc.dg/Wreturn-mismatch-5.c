/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic-errors" } */

void f1 (void);

int
f2 (void)
{
  f1 ();
}

static inline int
f3 (void)
{
  f1 ();
}

void
f4 (void)
{
  return 1; /* { dg-error "with a value,\[^\n\r\]*-Wreturn-mismatch" } */
}

void
f5 (void)
{
  return f1 (); /* { dg-error "with expression\[^\n\r\]*-Wpedantic" } */
}

int
f6 (void)
{
  return;
}

int
f7 (void)
{
  return f1 (); /* { dg-error "void value not ignored as it ought to be" } */
}
