/* { dg-do compile } */

void ice_mult32 (int x)
{
  register long reg __asm ("22");
  __asm volatile (" " :: "r" (reg = 0x12345 * x));
}

void ice_mult24 (int x)
{
  register __int24 reg __asm ("20");
  __asm volatile (" " :: "r" (reg = 0x12345 * x));
}

void ice_sh24 (__int24 x)
{
  register __int24 reg __asm ("20");
  __asm volatile (" " :: "r" (reg = x << 3));
}

void ice_sh24b (__int24 x)
{
  register __int24 reg __asm ("20");
  __asm volatile (" " :: "r" (reg = x << 22));
}

void ice_s16s16 (int x)
{
  register long reg __asm ("20");
  __asm volatile (" " :: "r" (reg = (long) x*x));
}

void ice_u16s16 (int x)
{
  register long reg __asm ("20");
  __asm volatile (" " :: "r" (reg = (long) x*0x1234u));
}
