/* { dg-do compile } */
/* { dg-options "-O3" } */

void
foo (unsigned char *dst, unsigned char *src)
{
  for (int y = 0; y < 16; y++)
    {
      for (int x = 0; x < 16; x++)
        dst[x] = src[x] + 1;
      dst += 32;
      src += 32;
    }
}
