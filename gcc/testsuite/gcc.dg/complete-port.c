/* This small program uses all the arithmetic operators that may
   generate calls to library routines which must be implemented in
   port-specific assembly language.  */
/* { dg-do link } */

#include <stddef.h>

int foo ();
double dfoo ();
void discard (int);
void ddiscard (double);

int
main (void)
{
  int a = foo (), b = foo ();
  unsigned int au = foo (), bu = foo ();
  float af = dfoo (), bf = dfoo ();
  double ad = dfoo (), bd = dfoo ();

  discard (a * b);
  discard (a / b);
  discard (a % b);

  discard (au / bu);
  discard (au % bu);

  discard (a >> b);
  discard (a << b);

  discard (au >> bu);
  discard (au << bu);

  ddiscard (ad + bd);
  ddiscard (ad - bd);
  ddiscard (ad * bd);
  ddiscard (ad / bd);
  ddiscard (-ad);

  ddiscard (af + bf);
  ddiscard (af - bf);
  ddiscard (af * bf);
  ddiscard (af / bf);
  ddiscard (-af);

  discard ((int) ad);
  discard ((int) af);

  ddiscard ((double) a);
  ddiscard ((float) a);
  ddiscard ((float) ad);

  discard (ad == bd);
  discard (ad < bd);
  discard (ad > bd);
  discard (ad != bd);
  discard (ad <= bd);
  discard (ad >= bd);

  discard (af == bf);
  discard (af < bf);
  discard (af > bf);
  discard (af != bf);
  discard (af <= bf);
  discard (af >= bf);

  return 0;
}

void
discard (x)
     int x __attribute__((__unused__));
{}

void
ddiscard (x)
     double x __attribute__((__unused__));
{}

int
foo ()
{
  static int table[] = {20, 69, 4, 12};
  static int idx;

  return table[idx++];
}

double
dfoo ()
{
  static double table[] = {20.4, 69.96, 4.4, 202.202};
  static int idx;

  return table[idx++];
}
