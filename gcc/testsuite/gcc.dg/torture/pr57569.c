/* { dg-do run } */

extern void abort (void) __attribute__((noreturn));

struct S { int f0; } a; 

int b, e, *d = &b, f;

void 
fn1 ()
{
  int **g[9][6];
  int ***h = &g[6][3];
  for (; e < 9; e++) {
    f = 0;
    for (; f < 6; f++)
      g[e][f] = &d;
  }
  ***h = 0;
}

void
fn2 ()
{
  fn1 ();
  struct S c[4][10] = {};
  a = c[3][9];
}

int
main ()
{
  fn2 ();
  if (a.f0 != 0)
    abort ();
  return 0;
}
