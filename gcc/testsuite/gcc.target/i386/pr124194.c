/* PR target/124194 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
typedef __attribute__((__vector_size__ (8 *sizeof (int)))) int V;

int
main ()
{
  unsigned char x = __builtin_ia32_cmpd256_mask ((V){}, (V){}, 7, 0xff);
  if (x != 0xff)
    __builtin_abort();
  return 0;
}
/* { dg-final { scan-assembler-times "xorl" 1 } } */
/* { dg-final { scan-assembler-not "vpcmpd" } } */
