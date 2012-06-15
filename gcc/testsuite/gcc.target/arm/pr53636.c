/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O -ftree-vectorize" } */
/* { dg-add-options arm_neon } */

void fill (short *buf) __attribute__ ((noinline));
void fill (short *buf)
{
  int i;

  for (i = 0; i < 11 * 8; i++)
    buf[i] = i;
}

void test (unsigned char *dst) __attribute__ ((noinline));
void test (unsigned char *dst)
{
  short tmp[11 * 8], *tptr;
  int i;

  fill (tmp);

  tptr = tmp;
  for (i = 0; i < 8; i++)
    {
      dst[0] = (-tptr[0] + 9 * tptr[0 + 1] + 9 * tptr[0 + 2] - tptr[0 + 3]) >> 7;
      dst[1] = (-tptr[1] + 9 * tptr[1 + 1] + 9 * tptr[1 + 2] - tptr[1 + 3]) >> 7;
      dst[2] = (-tptr[2] + 9 * tptr[2 + 1] + 9 * tptr[2 + 2] - tptr[2 + 3]) >> 7;
      dst[3] = (-tptr[3] + 9 * tptr[3 + 1] + 9 * tptr[3 + 2] - tptr[3 + 3]) >> 7;
      dst[4] = (-tptr[4] + 9 * tptr[4 + 1] + 9 * tptr[4 + 2] - tptr[4 + 3]) >> 7;
      dst[5] = (-tptr[5] + 9 * tptr[5 + 1] + 9 * tptr[5 + 2] - tptr[5 + 3]) >> 7;
      dst[6] = (-tptr[6] + 9 * tptr[6 + 1] + 9 * tptr[6 + 2] - tptr[6 + 3]) >> 7;
      dst[7] = (-tptr[7] + 9 * tptr[7 + 1] + 9 * tptr[7 + 2] - tptr[7 + 3]) >> 7;

      dst += 8;
      tptr += 11;
    }
}

int main (void)
{
  char buf [8 * 8];

  test (buf);

  return 0;
}

