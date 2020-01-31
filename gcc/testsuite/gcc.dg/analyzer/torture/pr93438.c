/* { dg-additional-options "-Wno-analyzer-too-complex" } */

void
tw (int **la, int pk)
{
  int *ow = *la;
  int jo = !!pk;

  if (jo == 0)
    *la = &pk;

  tw (&ow, pk);
}
