/* { dg-do run } */

extern void abort (void);
struct { int f1; } a[2];

int *b, *const k = &a[1].f1;
static int **c = &b;

int e, f, d;

int
main ()
{
  int **l = &b;
  *l = k;
  for (; d <= 0; d++)
    {
      int *j = &e;
      **c = 1;
      *l = k;
      *k ^= 0;
      f = **l;
      *j = f;
    }
  if (e != 1)
    abort ();
  return 0;
}

