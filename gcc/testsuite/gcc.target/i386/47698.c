/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "cmov" } } */

extern volatile unsigned long mmio;
unsigned long foo(int cond)
{
      if (cond)
              return mmio;
        return 0;
}
