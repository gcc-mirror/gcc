/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void
f1 (void)
{
  extern int extvari;
  extvari = 1;
}

int extvarj;

void
f2 (void)
{
  extern int extvarj;
  extvarj = 1;
}

static int extvark;

void
f3 (void)
{
  extern int extvark;
  extvark = 1;
}

int
f4 (void)
{
  return extvark;
}
