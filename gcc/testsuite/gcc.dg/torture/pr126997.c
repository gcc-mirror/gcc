/* { dg-do compile } */

unsigned short
f (unsigned int hl, unsigned int scheme, unsigned int n)
{
  unsigned short tot = 0;
  for (unsigned short i = 0; i < n; i++)
    tot += hl == 1 ? (scheme != 0) : (i == 0);
  return tot;
}
