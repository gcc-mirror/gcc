/* Test C2y static assertions in expressions: -Wc23-c2y-compat warnings for
   C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc23-c2y-compat" } */

/* Old forms of static assertion still valid.  */
static_assert (1);
static_assert (2, "message" );
struct s { int a; static_assert (3); };

void
f ()
{
  static_assert (4);
 label:
  static_assert (5);
  for (static_assert (6);;)
    ;
}

/* Test new forms of static assertion.  */
void
g ()
{
  (void) 0, static_assert (7), (void) 0; /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  extern typeof (static_assert (8)) f (); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  1
    ? static_assert (9) /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
    : static_assert (10); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  if (1)
    static_assert (11); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  else
    static_assert (12); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  for (;;)
    static_assert (13); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  while (true)
    static_assert (14); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  do
    static_assert (15); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  while (false);
  switch (16)
    static_assert (17); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  (void) static_assert (18); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  (static_assert (19)); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
}

void
g2 ()
{
  (void) 0, static_assert (7, "message"), (void) 0; /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  extern typeof (static_assert (8, "message")) f (); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  1
    ? static_assert (9, "message") /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
    : static_assert (10, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  if (1)
    static_assert (11, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  else
    static_assert (12, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  for (;;)
    static_assert (13, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  while (true)
    static_assert (14, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  do
    static_assert (15, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  while (false);
  switch (16)
    static_assert (17, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  (void) static_assert (18, "message"); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
  (static_assert (19, "message")); /* { dg-warning "ISO C does not support static assertions in expressions before C2Y" } */
}
