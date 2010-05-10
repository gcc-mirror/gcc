/* { dg-do compile } */
/* { dg-options "-O -fgcse" } */

extern void fe ();

extern int i;

static inline void
FX (void (*f) ())
{
  fe ();
  (*f) ();
}

static inline void
f4 ()
{
  for (;;)
    switch (i)
      {
      case 306:
      FX (&fe);
      break;
      default:
      return;
      }
}

static inline void
f3 ()
{
  f4 ();
  for (;;)
    switch (i)
      {
      case 267:
      FX (&f4);
      break;
      default:
      return;
      }
}

static inline void
f2 ()
{
  f3 ();
  while (i)
    FX (&f3);
}

void
f1 ()
{
  f2 ();
  while (1)
    FX (&f2);
}
