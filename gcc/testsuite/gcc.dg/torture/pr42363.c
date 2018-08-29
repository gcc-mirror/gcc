/* PR middle-end/pr42363, extended from the test for PR middle-end/37913.  */
/* { dg-do compile } */
/* { dg-options "-g" } */

void foo (void) __attribute__ ((noreturn));

static int __attribute__ ((noreturn))
bar (void)
{
  foo ();
}

int
baz (void)
{
  int i = bar ();
  return i + 1;
}

int fooz (void) __attribute__ ((noreturn));

static int __attribute__ ((noreturn))
bart (void)
{
  return fooz (); /* { dg-warning "noreturn" } */
}

int bazr (void)
{
  int i = bart ();
  return i + 1;
}

static inline int
bard (void)
{
  return fooz ();
}

int bizr (void)
{
  int i, j;

  i = j = bard ();

  return i + 1;
}

/* This might be regarded as pure and folded, rather than inlined,
   but because it's pure evil it's diagnosed and the noreturn attribute
   is dropped.  The const attribute is dropped as well because it's
   mutually exclusive with pure.  */
static int __attribute__ ((pure, const, noreturn))
barf (void) {
  /* { dg-warning "ignoring attribute .const." "const" { target *-*-* } .-1 } */
  /* { dg-warning "ignoring attribute .noreturn." "noreturn" { target *-*-* } .-2 } */

  /* The noreturn attribute is ignored so verify there is no warning
     for returning from the function:
     { dg-bogus "does return" } */
}

static int __attribute__ ((pure, const))
bark (void) {   /* { dg-warning "ignoring attribute .const." } */
  barf ();
}

int buzr (void)
{
  int i, j;

  i = j = bark () + bark ();

  return i + 1;
}

int buzt (void)
{
  int i, j;

  i = j = barf () + barf ();

  return i + 1;
}

void bust (void)
{
  while (barf ())
    ;
}
