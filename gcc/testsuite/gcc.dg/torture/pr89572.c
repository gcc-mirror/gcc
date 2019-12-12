/* { dg-do compile } */
/* { dg-additional-options "-finline-functions" } */

int vh, it, k1;

void
vn (void)
{
  ++vh;
  if (vh == 0 && it == 0)
    k1 = -k1;
}

__attribute__ ((returns_twice)) void
ef (int *uw)
{
  while (uw != (void *) 0)
    {
      vn ();
      *uw = 0;
    }
}

void
gu (int *uw)
{
  ef (uw);
}
