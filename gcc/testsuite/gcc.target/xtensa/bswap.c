/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned long f32(unsigned long v)
{
  return __builtin_bswap32(v);
}

unsigned long long f64(unsigned long long v)
{
  return __builtin_bswap64(v);
}

/* { dg-final { scan-assembler-times "ssai" 2 } } */
