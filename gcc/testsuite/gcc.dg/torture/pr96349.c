/* { dg-do compile } */

void __attribute__ ((returns_twice))
gr (void);

void
ib (void);

void
zg (void);

void
yw (int uz)
{
  gr ();

  for (;;)
    if (uz != 0)
      {
        uz = 0;
        ib ();
      }
    else
      zg ();
}
