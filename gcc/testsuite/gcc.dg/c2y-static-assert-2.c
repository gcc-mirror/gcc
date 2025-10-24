/* Test C2y static assertions in expressions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

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
  (void) 0, static_assert (7), (void) 0;
  extern typeof (static_assert (8)) f ();
  1 ? static_assert (9) : static_assert (10);
  if (1)
    static_assert (11);
  else
    static_assert (12);
  for (;;)
    static_assert (13);
  while (true)
    static_assert (14);
  do
    static_assert (15);
  while (false);
  switch (16)
    static_assert (17);
  (void) static_assert (18);
  (static_assert (19));
}

void
g2 ()
{
  (void) 0, static_assert (7, "message"), (void) 0;
  extern typeof (static_assert (8, "message")) f ();
  1 ? static_assert (9, "message") : static_assert (10, "message");
  if (1)
    static_assert (11, "message");
  else
    static_assert (12, "message");
  for (;;)
    static_assert (13, "message");
  while (true)
    static_assert (14, "message");
  do
    static_assert (15, "message");
  while (false);
  switch (16)
    static_assert (17, "message");
  (void) static_assert (18, "message");
  (static_assert (19, "message"));
}
