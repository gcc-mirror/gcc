/* Test for VLA size evaluation; see PR 35198.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-std=c99" } */

extern void exit (int);
extern void abort (void);

int i;
void *p;

void
f1 (void *x, int j)
{
  p = (int (*)[++i])x;
  if (i != j)
    abort ();
}

void
f1c (void *x, int j)
{
  p = (int (*)[++i]){x};
  if (i != j)
    abort ();
}

void
f2 (void *x, int j)
{
  x = (void *)(int (*)[++i])p;
  if (i != j)
    abort ();
}

void
f2c (void *x, int j)
{
  x = (void *)(int (*)[++i]){p};
  if (i != j)
    abort ();
}

void
f3 (void *x, int j)
{
  (void)(int (*)[++i])p;
  if (i != j)
    abort ();
}

void
f3c (void *x, int j)
{
  (void)(int (*)[++i]){p};
  if (i != j)
    abort ();
}

void
f4 (void *x, int j)
{
  (int (*)[++i])p;
  (int (*)[++i])p;
  if (i != j)
    abort ();
}

void
f4c (void *x, int j)
{
  (int (*)[++i]){p};
  (int (*)[++i]){p};
  if (i != j)
    abort ();
}

void
f5c (void *x, int j, int k)
{
  (++i, f3c (x, j), (int (*)[++i]){p});
  if (i != k)
    abort ();
}

int
main (void)
{
  f1 (p, 1);
  f2 (p, 2);
  f3 (p, 3);
  f4 (p, 5);
  f1c (p, 6);
  f2c (p, 7);
  f3c (p, 8);
  f4c (p, 10);
  f5c (p, 12, 13);
  exit (0);
}
