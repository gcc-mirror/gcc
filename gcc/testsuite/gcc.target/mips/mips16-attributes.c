/* Verify that mips16 and nomips16 attributes work, checking all combinations
   of calling a nomips16/mips16/default function from a nomips16/mips16/default
   function.  */
/* { dg-do run } */
/* { dg-options "(-mips16) (REQUIRES_STDLIB)" } */

#include <stdlib.h>

#define ATTR1 __attribute__ ((nomips16))
#define ATTR2 __attribute__ ((mips16))
#define ATTR3

double ATTR1
f1 (int i, float f, double d)
{
  return i + f + d;
}

double ATTR2
f2 (int i, float f, double d)
{
  return i + f + d;
}

double ATTR3
f3 (int i, float f, double d)
{
  return i + f + d;
}

void ATTR1
g1 (int i, float f, double d)
{
  double r = i + f + d;

  if (f1 (i, f, d) != r)
    abort ();
  if (f2 (i+1, f+1, d+1) != r + 3)
    abort ();
  if (f3 (i+2, f+2, d+2) != r + 6)
    abort ();
}

void ATTR2
g2 (int i, float f, double d)
{
  double r = i + f + d;

  if (f1 (i, f, d) != r)
    abort ();
  if (f2 (i+1, f+1, d+1) != r + 3)
    abort ();
  if (f3 (i+2, f+2, d+2) != r + 6)
    abort ();
}

void ATTR3
g3 (int i, float f, double d)
{
  double r = i + f + d;

  if (f1 (i, f, d) != r)
    abort ();
  if (f2 (i+1, f+1, d+1) != r + 3)
    abort ();
  if (f3 (i+2, f+2, d+2) != r + 6)
    abort ();
}

int ATTR3
main (void)
{
  int i = 1;
  float f = -2.0;
  double d = 3.0;

  g1 (i, f, d);
  g2 (i, f, d);
  g3 (i, f, d);

  exit (0);
}

