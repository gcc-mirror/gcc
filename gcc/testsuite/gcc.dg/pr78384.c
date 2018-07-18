/* PR tree-optimization/78384
   { dg-do compile }
   { dg-options "-O3 -w -fsplit-loops" } */
void
a2 (int wv, int yg, int r9)
{
  while (wv < 1)
    {
      int vn = r9 % 0;

      while (yg < 1)
        {
          int lz;

          for (r9 = 0; r9 < 17; ++r9)
            {
            }

 it:
          lz = (yg++ >= 0) ? 2 : 0;
          wv = vn < lz;
        }
    }
  goto it;
}
