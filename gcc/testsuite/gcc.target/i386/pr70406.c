/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O -mtune=pentium2 -mavx512f" } */

typedef int v4si __attribute__ ((vector_size (16)));

unsigned
foo (unsigned char i, unsigned x, v4si u, v4si v, v4si w)
{
  i &= (unsigned)~x;
  v <<= w[x];
  return i + u[x] + v[i];
}
