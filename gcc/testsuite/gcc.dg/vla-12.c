/* Test for typeof evaluation: should be at the appropriate point in
   the containing expression rather than just adding a statement.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

extern void exit (int);
extern void abort (void);

void *p;

void
f1 (void)
{
  int i = 0, j = -1, k = -1;
  /* typeof applied to expression with cast.  */
  (j = ++i), (void)(typeof ((int (*)[(k = ++i)])p))p;
  if (j != 1 || k != 2 || i != 2)
    abort ();
}

void
f2 (void)
{
  int i = 0, j = -1, k = -1;
  /* typeof applied to type.  */
  (j = ++i), (void)(typeof (int (*)[(k = ++i)]))p;
  if (j != 1 || k != 2 || i != 2)
    abort ();
}

void
f3 (void)
{
  int i = 0, j = -1, k = -1;
  void *q;
  /* typeof applied to expression with cast that is used.  */
  (j = ++i), (void)((typeof (1 + (int (*)[(k = ++i)])p))p);
  if (j != 1 || k != 2 || i != 2)
    abort ();
}

int
main (void)
{
  f1 ();
  f2 ();
  f3 ();
  exit (0);
}
