/* { dg-additional-options "-O1" } */

void
wb (_Complex double jh)
{
  _Complex double af = 0.0;

  do
    {
      af += jh;
    }
  while (af != 0.0);
}

_Complex double
o6 (void)
{
  _Complex double ba = 0.0;

  for (;;)
    {
      wb (ba);
      ba = 1.0;
    }

  return ba;
}
