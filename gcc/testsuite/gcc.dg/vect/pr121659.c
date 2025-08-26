/* { dg-do compile } */

_Bool a;
void b(_Bool c[][3])
{
  for (short d = 0; d < 100; d++)
    for (int e = 1; e < 21; e += 4)
      a ^= !c[1][1];
  for (;;)
    ;
}
