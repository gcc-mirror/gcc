/* { dg-do compile } */
/* { dg-additional-options "-fwrapv" } */

signed char rq;

signed char
pu (int tr, int al)
{
  signed char x8;

  while (tr != 0)
    {
      for (x8 = 0; x8 >= 0; x8 += 2)
        ;

      rq ^= al ^ 1;
      ++x8;
      ++tr;
    }

  return x8;
}
