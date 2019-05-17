/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-fre -Wuninitialized" } */
int pz;
int zi;

void
uk (void)
{
  int th = 1;
  int *gw = &zi;

  for (zi = 0; zi < 2; ++zi)
    {
      int a2 = 0;

      for (zi = 0; zi < 1; ++zi)
        {
          th = a2 * 2;

 og:
          for (pz = 0; pz < 1; ++pz)
            {
            }
        }

      pz = !!*gw ? *gw : pz;
      pz = (!!th ? (pz & 1) : 0);
      if (pz == 0)
        ++a2;
    }

  goto og;
}
