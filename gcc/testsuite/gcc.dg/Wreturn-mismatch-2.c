/* { dg-do compile } */
/* { dg-options "-fpermissive -Wall" } */

void f1 (void);

int
f2 (void)
{
  f1 ();
} /* { dg-warning "control reaches end of non-void\[^\n\r\]*-Wreturn-type" } */

static inline int
f3 (void)
{
  f1 ();
} /* { dg-warning "no return statement in function\[^\n\r\]*-Wreturn-type" } */

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
  return; /* { dg-warning "with no value,\[^\n\r\]*Wreturn-mismatch" } */
}

int
f7 (void)
{
  return f1 (); /* { dg-error "void value not ignored as it ought to be" } */
} /* { dg-bogus "control reaches end of non-void\[^\n\r\]*-Wreturn-type" } */

