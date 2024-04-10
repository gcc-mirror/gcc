/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-w" } */

typedef const unsigned char *It;
It DecodeSLEB128(It begin, It end, int *v) {
  int value = 0;
  unsigned shift = 0;
  unsigned char byte;
  do
  {
    if (begin == end)
      return begin;
    byte = *(begin++);
    int slice = byte & 0x7f;
    value |= slice << shift;
  } while (byte >= 128);
  *v = value;
  return begin;
}
