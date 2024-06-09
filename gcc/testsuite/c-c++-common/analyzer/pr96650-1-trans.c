/* { dg-additional-options "-O2 -fanalyzer-transitivity" } */

int *wf;

void
yd (void);

int
cy (void);

int *
ee (int hp)
{
  if (hp != 0)
    yd ();

  return 0;
}

void
z0 (int co)
{
  int l4 = sizeof (int);

 aq:
  wf = ee (l4);
  if (l4 < co)
    l4 = cy () + sizeof (int);
  goto aq;
}
