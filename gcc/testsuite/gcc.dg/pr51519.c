/* { dg-do compile } */
/* { dg-options "-O -fno-guess-branch-probability -findirect-inlining" } */

void fe (void);
int i;

static inline void
FX (void (*f) (void))
{
  fe ();
  (*f) ();
}

static inline void
f4 ()
{
  if (i)
    FX (fe);
}

static inline void
f3 (void)
{
  f4 ();
  if (i)
    FX (f4);
}

static inline void
f2 (void)
{
  FX (&f3);
}

void
f1 (void)
{
  FX (&f2);
}
