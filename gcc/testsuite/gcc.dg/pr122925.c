/* { dg-do run } */
/* { dg-options "-O2" } */

typedef __UINT64_TYPE__ uint64_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT16_TYPE__ uint16_t;

__attribute__ ((noipa)) int
foo (uint32_t a, uint16_t b)
{
  uint64_t c = (b ^ -4);
  if (1 - c >= b)
    a = 100;
  return a;
}

int
main (void)
{
  if (foo (0, -1))
    __builtin_abort ();
  return 0;
}
