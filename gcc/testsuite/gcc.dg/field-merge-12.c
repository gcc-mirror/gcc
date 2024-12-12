/* { dg-do run } */
/* { dg-options "-O2" } */

/* Check that we don't crash when trying to handle masks that don't match the
   width of the original type.  */

struct s {
  long long q;
};

struct s x1 = { 1 };
struct s xm1 = { -1 };
struct s x8 = { 8 };
struct s x0 = { 0 };

bool f(struct s *p)
{
  int q = (int)p->q;
  return (q < 0) || (q & 7);
}

int main ()
{
  if (!f (&x1))
    __builtin_abort ();
  if (!f (&xm1))
    __builtin_abort ();
  if (f (&x8))
    __builtin_abort ();
  if (f (&x0))
    __builtin_abort ();
  return 0;
}
