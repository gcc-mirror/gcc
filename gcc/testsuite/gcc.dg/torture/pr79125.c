int za;

void
hl (void)
{
  short int o8 = 0;
  short int m6 = 1;
  short int *ni = &m6;

  for (;;)
    {
      int af;
      short int *fd = (short int *)&ni;

      if (ni != 0)
        {
          if (m6 != 0)
            *ni = 0;
          else
            za = 0;
          af = (o8 * o8) || o8;
          if (af == 0)
            m6 /= 0; /* { dg-warning "division" } */
          while (za != 0)
            {
            }
        }
      *fd = &o8; /* { dg-warning "without a cast" } */
      for (af = 0; af < 2; ++af)
        af = za;
    }
}
