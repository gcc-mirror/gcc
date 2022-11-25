/* { dg-do compile } */
/* { dg-additional-options "-O3 -msve-vector-bits=512" } */

void f(unsigned short *restrict p1, unsigned int *restrict p2)
{
    for (int i = 0; i < 16; ++i)
      {
        p1[i] /= 0xff;
        p2[i] += 1;
      }
}

