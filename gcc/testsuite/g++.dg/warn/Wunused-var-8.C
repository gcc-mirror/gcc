// { dg-do compile }
// { dg-options "-Wunused" }

template <int N>
void
f1 (void)
{
  extern int extvari;
  extvari = 1;
}

int extvarj;

template <int N>
void
f2 (void)
{
  extern int extvarj;
  extvarj = 1;
}

static int extvark;

template <int N>
void
f3 (void)
{
  extern int extvark;
  extvark = 1;
}

template <int N>
int
f4 (void)
{
  return extvark;
}

void
test ()
{
  f1<0> ();
  f2<0> ();
  f3<0> ();
  (void) f4<0> ();
}
