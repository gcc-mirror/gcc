/* PR target/83008 */
/* { dg-do compile } */
/* { dg-options "-Ofast -funroll-loops -march=skylake-avx512 -mfpmath=sse" } */
/* { dg-final { scan-assembler-not "vmovdq(a|u)(32|64)" } } */

int
pr83008 (unsigned char *pix1, int i_pix1, unsigned char *pix2, int i_pix2)
{
  unsigned int tmp[4][4];
  unsigned int a0, a1, a2, a3;
  int sum = 0;
  for (int i = 0; i < 4; i++, pix1 += i_pix1, pix2 += i_pix2)
    {
      a0 = (pix1[0] - pix2[0]) + ((pix1[4] - pix2[4]) << 16);
      a1 = (pix1[1] - pix2[1]) + ((pix1[5] - pix2[5]) << 16);
      a2 = (pix1[2] - pix2[2]) + ((pix1[6] - pix2[6]) << 16);
      a3 = (pix1[3] - pix2[3]) + ((pix1[7] - pix2[7]) << 16);
      int t0 = a0 + a1;
      int t1 = a0 - a1;
      int t2 = a2 + a3;
      int t3 = a2 - a3;
      tmp[i][0] = t0 + t2;
      tmp[i][2] = t0 - t2;
      tmp[i][1] = t1 + t3;
      tmp[i][3] = t1 - t3;
    }
  for (int i = 0; i < 4; i++)
    {
      int t0 = tmp[0][i] + tmp[1][i];
      int t1 = tmp[0][i] - tmp[1][i];
      int t2 = tmp[2][i] + tmp[3][i];
      int t3 = tmp[2][i] - tmp[3][i];
      a0 = t0 + t2;
      a2 = t0 - t2;
      a1 = t1 + t3;
      a3 = t1 - t3;
      sum += (a0) + (a1) + (a2) + (a3);
    }
  return (sum + ((unsigned int) sum >> 16)) >> 1;
}
