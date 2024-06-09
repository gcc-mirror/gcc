/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_zbb_xtheadmemidx -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32imafc_zbb_xtheadmemidx -mabi=ilp32f" { target { rv32 } } } */

const unsigned char *
read_uleb128(const unsigned char *p, unsigned long *val)
{
  unsigned int shift = 0;
  unsigned char byte;
  unsigned long result;

  result = 0;
  do
  {
    byte = *p++;
    result |= ((unsigned long)byte & 0x7f) << shift;
    shift += 7;
  } while (byte & 0x80);

  *val = result;
  return p;
}

void test(const unsigned char *p, unsigned long utmp)
{
  p = read_uleb128(p, &utmp);
}

/* { dg-final { scan-assembler-not {\mlb\ta[0-9],\(a[0-9]\),1,0\M} } } */
