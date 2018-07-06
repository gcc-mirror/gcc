/* PR debug/84404 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -floop-nest-optimize -g" } */

int te[9];

void
dt (int cz)
{
  while (cz < 1)
    {
      int xy;

      for (xy = 0; xy < 9; ++xy)
        te[xy] = 0;

      ++cz;
    }
}
