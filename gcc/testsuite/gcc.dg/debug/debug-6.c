/* Verify that bb-reorder re-inserts nested scopes properly.  */
/* { dg-do compile } */
/* { dg-options "-dA" } */
/* { dg-final { scan-assembler "xyzzy" } } */

extern void abort (void);

struct A { char *a, *b, *c, *d; };

static int
bar2 (struct A *x)
{
  int a = x->c - x->b;
  x->c += 26;
  return a;
}
   
void fnptr (int *);

void
foo (void)
{
  struct A e;

  if (bar2 (&e) < 0)
    abort ();
  {
    int xyzzy;
    fnptr (&xyzzy);
  }
  {
    struct A *f;
  
    f = &e;
    if (f->c - f->a > f->d - f->a)
      f->c = f->d;
  }
}
