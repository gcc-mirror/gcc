/* Test C2y static assertions in expressions: failed assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

/* Old forms of static assertion still valid.  */
static_assert (0); /* { dg-error "static assertion failed" } */
static_assert (0, "message" ); /* { dg-error "static assertion failed" } */
struct s { int a; static_assert (0); }; /* { dg-error "static assertion failed" } */

void
f ()
{
  static_assert (0); /* { dg-error "static assertion failed" } */
 label:
  static_assert (0); /* { dg-error "static assertion failed" } */
  for (static_assert (0);;) /* { dg-error "static assertion failed" } */
    ;
}

/* Test new forms of static assertion.  */
void
g ()
{
  (void) 0, static_assert (0), (void) 0; /* { dg-error "static assertion failed" } */
  extern typeof (static_assert (0)) f (); /* { dg-error "static assertion failed" } */
  1
    ? static_assert (0) /* { dg-error "static assertion failed" } */
    : static_assert (0); /* { dg-error "static assertion failed" } */
  if (1)
    static_assert (0); /* { dg-error "static assertion failed" } */
  else
    static_assert (0); /* { dg-error "static assertion failed" } */
  for (;;)
    static_assert (0); /* { dg-error "static assertion failed" } */
  while (true)
    static_assert (0); /* { dg-error "static assertion failed" } */
  do
    static_assert (0); /* { dg-error "static assertion failed" } */
  while (false);
  switch (16)
    static_assert (0); /* { dg-error "static assertion failed" } */
  (void) static_assert (0); /* { dg-error "static assertion failed" } */
  (static_assert (0)); /* { dg-error "static assertion failed" } */
}

void
g2 ()
{
  (void) 0, static_assert (0, "message"), (void) 0; /* { dg-error "static assertion failed" } */
  extern typeof (static_assert (0, "message")) f (); /* { dg-error "static assertion failed" } */
  1
    ? static_assert (0, "message") /* { dg-error "static assertion failed" } */
    : static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  if (1)
    static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  else
    static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  for (;;)
    static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  while (true)
    static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  do
    static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  while (false);
  switch (16)
    static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  (void) static_assert (0, "message"); /* { dg-error "static assertion failed" } */
  (static_assert (0, "message")); /* { dg-error "static assertion failed" } */
}
