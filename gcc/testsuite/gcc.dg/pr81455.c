/* { dg-do compile } */
/* { dg-options "-O -funswitch-loops" } */

void
jh (unsigned int aw, int sn)
{
  int xs;

  for (xs = 0; xs < 1; ++xs)
    aw &= 1;

  while (aw < 1 || ++sn < 1)
    {
    }
}
