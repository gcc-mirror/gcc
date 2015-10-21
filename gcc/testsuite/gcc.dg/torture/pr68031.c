/* { dg-do compile } */

void _mktm_r (long lcltime,  int *res)
{
  long rem = lcltime % ((60L * 60L) * 24L);
  if (rem < 0)
    return;
  while (rem >= ((60L * 60L) * 24L))
      rem -= ((60L * 60L) * 24L);
  *res = (int) (rem % 60L);
}
