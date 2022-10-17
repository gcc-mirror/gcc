/* { dg-do compile } */
/* { dg-additional-options "-march=bdver2" { target x86_64-*-* i?86-*-* } } */

int rl2GeomExport64_little_endian, rl2GeomExport64_little_endian_arch;
void rl2GeomExport64(unsigned char *p, double value) {
  union {
    unsigned char byte[8];
    double double_value;
  } convert;
  convert.double_value = value;
  if (rl2GeomExport64_little_endian_arch)
    if (rl2GeomExport64_little_endian) {
      *(p + 7) = convert.byte[0];
      *(p + 6) = convert.byte[1];
      *(p + 5) = convert.byte[2];
      *(p + 4) = convert.byte[3];
      *(p + 3) = convert.byte[4];
      *(p + 2) = convert.byte[5];
      *(p + 1) = convert.byte[6];
      *p = convert.byte[7];
    } else
      *p = convert.byte[7];
}
