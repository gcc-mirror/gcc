/* { dg-do compile }  */

int rj;

void __attribute__ ((returns_twice))
da (void)
{
  rj = 1;
}

void
c5 (void)
{
  for (;;)
    ++rj;
}

void
ls (int kz)
{
  if (kz == 0)
    {
      rj = 0;
      c5 ();
    }

  da ();
  c5 ();
}
