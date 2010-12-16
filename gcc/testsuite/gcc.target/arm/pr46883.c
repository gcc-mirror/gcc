/* { dg-do compile } */
/* { dg-options "-O1 -march=armv5te" } */

void bar (unsigned char *q, unsigned short *data16s, int len)
{
  int i;

  for (i = 0; i < len; i++)
    {
      q[2 * i] =
        (((data16s[i] & 0xFF) << 8) | ((data16s[i] >> 8) & 0xFF)) & 0xFF;
      q[2 * i + 1] =
        ((unsigned short)
         (((data16s[i] & 0xFF) << 8) | ((data16s[i] >> 8) & 0xFF))) >> 8;
    }
}
