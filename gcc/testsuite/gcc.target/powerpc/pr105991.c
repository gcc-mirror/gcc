/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target lp64 } */
unsigned long long
foo (unsigned long long value)
{
  value &= 0xffffffff;
  value |= value << 32;
  return value;
}
/* { dg-final { scan-assembler {\mrldimi\M} } } */
