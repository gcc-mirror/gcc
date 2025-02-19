/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64" } */
/* { dg-final { scan-assembler "t1: loongarch64" } } */
/* { dg-final { scan-assembler "t2: la64v1.1" } } */
/* { dg-final { scan-assembler "t3: loongarch64" } } */

#ifndef __loongarch_arch
#error __loongarch_arch should be available here
#endif

void
t1 (void)
{
  asm volatile ("# t1: " __loongarch_arch);
}

#pragma GCC push_options
#pragma GCC target("arch=la64v1.1")

void
t2 (void)
{
  asm volatile ("# t2: " __loongarch_arch);
}

#pragma GCC pop_options

void
t3 (void)
{
  asm volatile ("# t3: " __loongarch_arch);
}
