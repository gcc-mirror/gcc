int u4, lx, e0, zy, pz;

void
tb (int m6)
{
  for (pz = 0; pz < 1; ++pz)
    {
      for (zy = 0; zy < 1; ++zy)
        for (u4 = 0; u4 < 1; ++u4)
          for (e0 = 0; e0 < 1; ++e0)
            {
 as:
              for (;;)
                {
                }
            }

      if (e0 != 0)
        goto ql;

      if (0)
        {
 o3:
          for (lx = 0; lx < 1; ++lx)
            {
              m6 |= lx;
              if (m6 == 0)
                lx = 0;
 ql:
              ;
            }
          goto as;
        }
    }
  goto o3;
}

