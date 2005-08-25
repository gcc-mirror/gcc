/* { dg-do compile } */
/* { dg-options "-O2" } */
typedef int m64 __attribute__ ((__vector_size__ (8)));

void mmxCombineMaskU (m64 * mask, int width)
{
  while (--width >= 0)
    *mask++ = (m64) 0LL;
}

