/* Let gimple verifier check what SRA does to unions and single-field
   strucutres . */

struct sim_struct
{
  int x;
};

extern struct sim_struct get_x(void);

struct sim_struct foo (void)
{
  struct sim_struct simple;

  simple = get_x ();
  if (simple.x % 2)
    simple.x = 39;
  else
    simple.x -=8;

  return simple;
}

struct sim_cmplx
{
  _Complex double c;
};

extern struct sim_cmplx get_sc (void);

_Complex double foo_c (void)
{
  struct sim_cmplx simple;

  simple = get_sc ();
  if (__real__ simple.c > 200.3)
    __imag__ simple.c -= 2.4;

  return simple.c;
}


union sim_union
{
  int i;
  float d;
};

extern union sim_union get_y (void);

union sim_union bar (void)
{
  union sim_union simple;

  simple = get_y ();
  if (simple.d > 8.2)
    simple.i = 300;

  return simple;
}

extern int get_int (void);

int bar_i (void)
{
  union sim_union simple;

  simple = get_y ();
  if (simple.d > 8.2)
    simple.i = get_int ();

  return simple.i;
}
