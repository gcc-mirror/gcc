/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

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
  return 1; /* { dg-warning "with a value,\[^\n\r\]*-Wreturn-mismatch" } */
}

void
f5 (void)
{
  return f1 ();
}

int
f6 (void)
{
  return; /* { dg-bogus "with no value" }  */
}

int
f7 (void)
{
  return f1 (); /* { dg-error "void value not ignored as it ought to be" } */
}
