/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-sources "tailcall-8.c" } */

struct s { int x; };

int expected;
struct s *last_ptr;
struct s tmp;

void
start (int val, struct s *initial_last_ptr)
{
  expected = val;
  tmp.x = val;
  last_ptr = initial_last_ptr;
}

void
f_direct (struct s param)
{
  if (param.x != expected)
    __builtin_abort ();
}

void
f_indirect (struct s *ptr)
{
  if (ptr->x != expected)
    __builtin_abort ();
  last_ptr = ptr;
  ptr->x += 100;
}

void
f_void (void)
{
  if (last_ptr->x != expected + 100)
    __builtin_abort ();
}


void g1 (struct s);
void g2 (struct s *);
void g3 (struct s *);
void g4 (struct s *);
void g5 (struct s);
void g6 (struct s);
void g7 (struct s);
void g8 (struct s *);
void g9 (struct s *);

int
main (void)
{
  struct s g6_s = { 106 };

  start (1, 0);
  g1 (tmp);

  start (2, 0);
  g2 (&tmp);

  start (3, 0);
  g3 (&tmp);

  start (4, 0);
  g4 (&tmp);

  start (5, 0);
  g5 (tmp);

  start (6, &g6_s);
  g6 (tmp);

  start (7, 0);
  g7 (tmp);

  start (8, 0);
  g8 (&tmp);

  start (9, 0);
  g9 (&tmp);

  return 0;
}
