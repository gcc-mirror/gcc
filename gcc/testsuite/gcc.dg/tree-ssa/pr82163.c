/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c[4], d, e, f, g;

void h ()
{
  for (; a; a++)
    {
      c[a + 3] = g;
      if (b)
        c[a] = f;
      else
        {
          for (; d; d++)
            c[d + 3] = c[d];
          for (e = 1; e == 2; e++)
            ;
          if (e)
            break;
        }
    }
}
