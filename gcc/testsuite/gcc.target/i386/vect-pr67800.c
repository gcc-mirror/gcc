/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details" } */
/* { dg-additional-options "-msse4.2" } */

#define ubyte unsigned char
#define byte char

#define SCALE 8

#define R2Y (76)
#define G2Y (150)
#define B2Y (30)
#define R2I (127)
#define G2I (-59)
#define B2I (-68)
#define R2Q (51)
#define G2Q (-127)
#define B2Q (76)

void
convert(ubyte *in, ubyte *out, unsigned n)
{
  ubyte r, g, b;
  ubyte y = 0;
  byte i, q;

  while (--n) {
	  r = *in++;
	  g = *in++;
	  b = *in++;

      y = (ubyte)(((R2Y * r) + (G2Y * g) + (B2Y * b) + (1 << (SCALE - 1))) >> SCALE);
      i = (byte)(((R2I * r) + (G2I * g) + (B2I * b) + (1 << (SCALE - 1))) >> SCALE);
      q = (byte)(((R2Q * r) + (G2Q * g) + (B2Q * b) + (1 << (SCALE - 1))) >> SCALE);

      *out++ = y;
	  *out++ = i;
	  *out++ = q;
  }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
