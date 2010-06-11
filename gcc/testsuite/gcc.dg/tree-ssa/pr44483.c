/* { dg-do compile } */
/* { dg-options "-c -O3" { target *-*-* } } */

int ffesum (void) {
  int ch[4], ii, jj, kk;
  char asc[32];

  for (ii = 0; ii < 4; ii++)
    {
      for (jj = 0; jj < 4; jj++)
        ch[jj] = ii;
      for (kk = 0; kk < 13; kk++)
        for (jj = 0; jj < 4; jj += 2)
          if ((unsigned char) ch[jj] || (unsigned char) ch[jj + 1])
            ch[jj]++;
      for (jj = 0; jj < 4; jj++)
        asc[4 * jj + ii] = ch[jj];
    }
  return asc[0];
}
