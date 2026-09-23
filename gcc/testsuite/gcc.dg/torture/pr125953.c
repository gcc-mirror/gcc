/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.3-a+sve2" { target aarch64-*-* } } */

unsigned char *begin(unsigned char *out)
{
  int size_0_0_0;
  unsigned char *kRow = begin(out);
  unsigned char *currentRow = kRow;
  for (int x = 0; x < size_0_0_0; ++x) {
    char k = kRow ? kRow[x] : 5;
    out[2] = currentRow[2] * k / 255;
  }
  return out;
}
