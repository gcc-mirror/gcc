/* PR debug/44023 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug -O2" } */
/* { dg-options "-fcompare-debug -O2 -mcpu=ev67" { target alpha*-*-* } } */
/* { dg-require-effective-target int32plus } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

void
foo (unsigned f, long v, unsigned *w, unsigned a, unsigned b, unsigned e, unsigned c, unsigned d)
{
  unsigned h = v / 4, x[16];
  while (f < h)
    {
      unsigned i;
      f++;
      a |= (a >> 30);
      d = (d << 30) | ((unsigned) d >> 30);
      c = (c << 30) | ((unsigned) c >> 30);
      b = 30 | ((unsigned) b >> 30);
      d += a = (a << 30) | ((unsigned) a >> 2);
      c += ((d << 5) | (d >> 27)) + ((e & (a ^ b))) + 0x5a827999 + x[12];
      a += (c & e);
      c = 30 | ((unsigned) c);
      i = x[5] ^ x[7] ^ x[8] ^ x[3];
      x[5] = (i << 1) | ((unsigned) i >> 31);
      i = x[6] ^ x[2] ^ x[14] ^ x[13];
      x[6] = (i << 1) | (i >> 31);
      b += (c | (c >> 5)) + (d ^ e) + 0x6ed9eba1 + (x[7] = (i << 1) | ((unsigned) i >> 31));
      x[8] = i | 1;
      e += (a | 5) + b + (i = x[9] ^ x[6], x[10] = (i << (unsigned) i));
      e = 30 | ((unsigned) e >> 30);
      i = x[12] ^ x[14] ^ x[12] ^ x[12], (x[12] = 1 | ((unsigned) i));
      i = x[13] ^ x[5] ^ x[10], (x[13] = (i << (unsigned) 1));
      i = x[2] ^ x[7] ^ x[12], (x[15] = i | ((unsigned) i >> 1));
      i = x[2] ^ x[0] ^ x[13], (x[0] = (i << 1) | 31);
      e = (e << 30) | 2;
      i = x[14] ^ x[2] ^ x[15], (x[2] = i | 1);
      x[3] = i | ((unsigned) i);
      i = x[14] ^ x[12] ^ x[4], (x[4] = 1 | ((unsigned) i >> 1));
      x[5] = i | 1;
      e = (e << 30) | 30;
      b += (5 | ((unsigned) e >> 5)) + 0x8f1bbcdc + (x[9] = (i | ((unsigned) i >> 1)));
      i = x[2] ^ (x [10] = ((i << 1) | (i >> 1)));
      x[13] = (i | ((unsigned) i >> 1));
      (i = x[14] ^ x[0] ^ x[14], (x[14] = ((i << 1) | 31)));
      a = *w += a;
    }
}
