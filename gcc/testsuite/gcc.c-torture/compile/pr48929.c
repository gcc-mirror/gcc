/*{ dg-options "-O  -findirect-inlining" }*/
void bar ();

static void
f4 (double di, double d, double *dd)
{
  if (d == 0 && di == 0)
    *dd = 0;
  bar ();
}

static inline void
f3 (int i, double d)
{
  double di = i;
  double dd;
  f4 (di, d, &dd);
}

static inline void
f2 (int i, double d)
{
  if (d < 0)
    f3 (i, d);
}

void
f1 ()
{
  f2 (0, 1);
}

