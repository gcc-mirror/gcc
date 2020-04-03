/* { dg-do compile }  */
/* { dg-options "-O -floop-nest-optimize" } */

int *
eo (void);

void
g4 (int *nt)
{
  int dh, t2 = (__INTPTR_TYPE__)eo ();

  for (dh = 0; dh < 2; ++dh)
    {
      int m7;

      for (m7 = 0; m7 < t2; ++m7)
        nt[m7] = 0;
    }
}
