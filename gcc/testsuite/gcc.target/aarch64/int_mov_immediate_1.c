/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3" } */

long long f1(void)
{
  return 0xffff6666;
}

int f3(void)
{
  return 0xffff6666;
}


long f2(void)
{
  return 0x11110000ffff6666;
}

long f4(void)
{
  return 0x11110001ffff6666;
}

long f5(void)
{
  return 0x111100001ff6666;
}

long f6(void)
{
  return 0x00001111ffff6666;
}

long f7(void)
{
  return 0x000011116666ffff;
}

long f8(void)
{
  return 0x0f0011116666ffff;
}

/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, -39322"      1 } } */
/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, 4294927974"  3 } } */
/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, 1718026239"  1 } } */
/* { dg-final { scan-assembler-times "mov\tx\[0-9\]+, -2576941057" 1 } } */
/* { dg-final { scan-assembler-times "mov\tx\[0-9\]+, -39322"      1 } } */
/* { dg-final { scan-assembler-times "mov\tx\[0-9\]+, 26214"       1 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0xf00, lsl 48" 1 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x1111, lsl 48" 2 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x1000, lsl 32" 1 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x1111, lsl 32" 3 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x111, lsl 48"  1 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x1ff, lsl 16"  1 } } */
/* { dg-final { scan-assembler-times "movk\tx\[0-9\]+, 0x1, lsl 32"    1 } } */

