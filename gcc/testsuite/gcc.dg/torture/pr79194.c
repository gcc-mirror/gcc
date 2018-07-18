/* { dg-do compile } */

int iw, vr;

void
d9 (unsigned int j3, long long int f5, int kp)
{
  int *qb = &kp;

  if (kp != 0)
    {
      long long int oq;
      unsigned int tl = 0;

      for (j3 = 0; j3 < 1; ++j3)
        qb = &tl;
      goto ed;

 l7:
      oq = 1;
      while (oq < 2)
        oq *= j3;

 ed:
      do
        {
          oq -= *qb;
          if (oq != 0)
            {
              long long int ie = j3 & f5;
              int ws = (j3 != 0 && kp != 0);

              tl = ie > ws;
              iw = vr = tl;
            }
          else
            tl = (kp != 0 && (0 % 0) != 0); /* { dg-warning "division by zero" } */
        }
      while (tl != 0);
    }
  goto l7;
}
