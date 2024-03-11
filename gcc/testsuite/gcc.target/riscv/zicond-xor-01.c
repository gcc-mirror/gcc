/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

long xor1(long crc, long poly)
{
  if (crc & 1)
    crc ^= poly;

  return crc;
}

/* { dg-final { scan-assembler-times {\mczero.eqz\M} 1 } } */
/* { dg-final { scan-assembler-times "xor\t" 1 } } */
