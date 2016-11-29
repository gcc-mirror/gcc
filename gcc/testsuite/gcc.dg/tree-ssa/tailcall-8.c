/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

struct s { int x; };
void f_direct (struct s);
void f_indirect (struct s *);
void f_void (void);

/* Tail call.  */
void
g1 (struct s param)
{
  f_direct (param);
}

/* Tail call.  */
void
g2 (struct s *param_ptr)
{
  f_direct (*param_ptr);
}

/* Tail call.  */
void
g3 (struct s *param_ptr)
{
  f_indirect (param_ptr);
}

/* Tail call.  */
void
g4 (struct s *param_ptr)
{
  f_indirect (param_ptr);
  f_void ();
}

/* Tail call.  */
void
g5 (struct s param)
{
  struct s local = param;
  f_direct (local);
}

/* Tail call.  */
void
g6 (struct s param)
{
  struct s local = param;
  f_direct (local);
  f_void ();
}

/* Not a tail call.  */
void
g7 (struct s param)
{
  struct s local = param;
  f_indirect (&local);
}

/* Not a tail call.  */
void
g8 (struct s *param_ptr)
{
  struct s local = *param_ptr;
  f_indirect (&local);
}

/* Not a tail call.  */
void
g9 (struct s *param_ptr)
{
  struct s local = *param_ptr;
  f_indirect (&local);
  f_void ();
}

/* { dg-final { scan-tree-dump-times "Found tail call" 6 "tailc" } } */
