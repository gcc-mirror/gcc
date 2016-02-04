/* PR middle-end/68570 */
/* { dg-do compile } */

int a, d, e, f, h, i, k;

void
fn1 ()
{
  char m;
  for (;;)
    {
      for (;;)
        {
          e = f = 1;
          if (i)
            d = h = 0;
          else
            a = 0;
          break;
        }
      k = 0;
      if (f)
        a = 3;
      if (d)
        f = 0;
      if (a > (i < 1))
        {
          if (e)
            break;
        }
      else
        i = m;
      k = i ? a : i;
    }
}
