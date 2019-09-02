/* { dg-do compile } */

void
gm (int *);

__attribute__ ((returns_twice)) void
jg (void)
{
}

void
eb (void)
{
  int r6 = 0;

  if (r6 != 0)
    gm (&r6);
}

void
gm (int *r6)
{
  jg ();

  for (;;)
    {
      eb ();
      *r6 = 0;
    }
}
