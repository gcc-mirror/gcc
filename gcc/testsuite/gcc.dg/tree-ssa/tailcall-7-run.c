/* { dg-do run { target trampolines } } */
/* { dg-options "-O2" } */
/* { dg-additional-sources "tailcall-7.c" } */

struct s { int x; };
extern struct s global;

void g1 (void);
void g2 (void);
void g3 (struct s *);
struct s g4 (struct s);
struct s g5 (void);
struct s g6 (void);
struct s g7 (void);
struct s g8 (struct s *);
int g9 (struct s);
int g10 (int);

struct s last;
struct s tmp;

struct s
f (int i)
{
  struct s ret;
  ret.x = i + 100;
  last = ret;
  return ret;
}

void
callit (void (*fn) (void))
{
  fn ();
}

int
test (int last_val, int global_val, int tmp_val)
{
  return last.x == last_val && global.x == global_val && tmp.x == tmp_val;
}

int
main (void)
{
  global.x = 200;
  tmp.x = 300;
  g1 ();
  if (!test (101, 200, 300))
    __builtin_abort ();
  g2 ();
  if (!test (102, 102, 300))
    __builtin_abort ();
  g3 (&tmp);
  if (!test (103, 102, 103))
    __builtin_abort ();
  if (g4 (tmp).x != 104 || !test (104, 102, 103))
    __builtin_abort ();
  if (g5 ().x != 105 || !test (105, 102, 103))
    __builtin_abort ();
  if (g6 ().x != 106 || !test (106, 102, 103))
    __builtin_abort ();
  if (g7 ().x != 107 || !test (107, 107, 103))
    __builtin_abort ();
  if (g8 (&tmp).x != 108 || !test (108, 107, 108))
    __builtin_abort ();
  if (g9 (tmp) != 9 || !test (109, 107, 108))
    __builtin_abort ();
  if (g10 (10) != 10 || !test (110, 107, 108))
    __builtin_abort ();
  return 0;
}
