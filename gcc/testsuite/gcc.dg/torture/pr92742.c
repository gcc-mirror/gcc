/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned int qw;

int
rs (int iq, int wg)
{
  for (qw = 0; qw < 2; ++qw)
    {
    }

  while (iq < 1)
    {
      wg *= qw * 2;
      ++iq;
    }

  return wg;
}
