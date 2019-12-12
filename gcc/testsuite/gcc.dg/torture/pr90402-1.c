/* { dg-do compile } */
/* { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } } */

int kn, ha;

int
c7 (void)
{
}

void
ul (int w3)
{
  kn = c7 ();

  while (w3 < 1)
    {
      ha += !!kn ? 1 : w3;

      for (kn = 0; kn < 2; ++kn)
	{
	}

      ++w3;
    }
}
