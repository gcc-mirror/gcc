/* { dg-do compile { target rv64 } } */
/* { dg-skip-if "" { *-*-* } { "-O0"} } */



unsigned
f4 (unsigned *p)
{
  unsigned s1 = p[0], s2 = p[1];
  unsigned b = ((s1 << 13) ^ s1) >> 19;
  s1 = ((s1 & 0xFFFFFFFEU) << 12) ^ b;
  b = ((s2 << 2) ^ s2) >> 25;
  s2 = ((s2 & 0xFFFFFFF8U) << 4) ^ b;
  p[0] = s1;
  p[1] = s2;
  return s1 ^ s2;
}

/* { dg-final { scan-assembler-not "sext.w\t" } } */

