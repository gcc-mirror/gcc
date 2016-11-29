/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

volatile int a;
int c, d, e, f, g, h;

int fn1 ()
{ 
  int i;
  for (; d;)
    { 
      if (e)
        break;
      g = 0;
      int j[4];
      for (h = 0; h < 4; h++)
        g++;
      for (; c < 2; c++)
        { 
          e = j[g];
          i = j[0];
          f = 4;
        }
      f |= d;
    }
  return a;
}
