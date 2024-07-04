/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

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
  return 1; /* { dg-warning "'return' with a value\[^\n\r\]*-Wreturn-mismatch" } */
}

void
f5 (void)
{
  return f1 (); /* { dg-bogus "ISO C" } */
}

int
f6 (void)
{
  return; /* { dg-warning "'return' with no value\[^\n\r\]*-Wreturn-mismatch" } */
}

int
f7 (void)
{
  return f1 (); /* { dg-error "void value not ignored as it ought to be" } */
}
