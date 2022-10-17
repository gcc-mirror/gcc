/* PR middle-end/101671 - pr83510 fails with -Os because threader confuses
   -Warray-bounds
   { dg-do compile }
   { dg-options "-Os -Wall" } */

extern int f (void);
extern void sink (unsigned int);

unsigned int a[10];

static unsigned int g (int i, int j)
{
  if (i == 9)
    return j;
  else if (i == 10)
    return a[i];    // no warning here
  return 0;
}

void test_g (int j)
{
  for (int i = 0; i < 10; i++)
    {
      if (f ())
	sink (g (i, j));
    }
}

static unsigned int h (int i, int j)
{
  switch (i)
    {
    case 9:
      return j;
    case 10:
      return a[i];  // { dg-bogus "-Warray-bounds" "pr101671" }
    }
  return 0;
}

void test_h (int j)
{
  for (int i = 0; i < 10; i++)
    {
      if (f ())
	sink (h (i, j));
    }
}
