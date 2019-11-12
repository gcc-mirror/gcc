/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

short int zb;

void
gs (void)
{
  while (zb < 1)
    {
      int at;

      zb %= 1;

      for (at = 0; at < 56; ++at)
	zb += zb;

      ++zb;
    }
}
