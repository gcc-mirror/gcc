/* { dg-do compile } */
/* { dg-options "-ftree-vectorize" } */

struct Pixel {
    unsigned short r;
    unsigned short g;
    unsigned short b;
    unsigned short a;
};

void fn(unsigned char * __restrict dst, const unsigned char * __restrict src)
{
  unsigned x;
  for(x = 0; x < 1024; x += 1)
    {
      struct Pixel pixel;
      pixel.r = (unsigned short)(((unsigned)src[0]) * 0xffff / 0xff);
      pixel.g = (unsigned short)(((unsigned)src[1]) * 0xffff / 0xff);
      pixel.b = (unsigned short)(((unsigned)src[2]) * 0xffff / 0xff);
      pixel.a = (unsigned short)(((unsigned)src[3]) * 0xffff / 0xff);
      __builtin_memcpy(dst, &pixel, sizeof pixel);
      src += 4;
      dst += 8;
    }
}
