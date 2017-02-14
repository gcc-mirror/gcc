/* { dg-do compile { target trampolines } } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

struct s { int x; };
struct s f (int);
struct s global;
void callit (void (*) (void));

/* Tail call.  */
void
g1 (void)
{
  f (1);
}

/* Not a tail call.  */
void
g2 (void)
{
  global = f (2);
}

/* Not a tail call.  */
void
g3 (struct s *ptr)
{
  *ptr = f (3);
}

/* Tail call.  */
struct s
g4 (struct s param)
{
  param = f (4);
  return param;
}

/* Tail call.  */
struct s
g5 (void)
{
  struct s local = f (5);
  return local;
}

/* Tail call.  */
struct s
g6 (void)
{
  return f (6);
}

/* Not a tail call.  */
struct s
g7 (void)
{
  struct s local = f (7);
  global = local;
  return local;
}

/* Not a tail call.  */
struct s
g8 (struct s *ptr)
{
  struct s local = f (8);
  *ptr = local;
  return local;
}

/* Not a tail call.  */
int
g9 (struct s param)
{
  void inner (void) { param = f (9); }
  callit (inner);
  return 9;
}

/* Tail call.  */
int
g10 (int param)
{
  void inner (void) { f (param); }
  callit (inner);
  return 10;
}

/* { dg-final { scan-tree-dump-times "Found tail call" 5 "tailc" } } */
