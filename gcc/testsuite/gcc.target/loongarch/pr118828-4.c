/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mtune=la464" } */
/* { dg-final { scan-assembler "t1: la464" } } */
/* { dg-final { scan-assembler "t2: la664" } } */
/* { dg-final { scan-assembler "t3: la464" } } */

#ifndef __loongarch_tune
#error __loongarch_tune should be available here
#endif

void
t1 (void)
{
  asm volatile ("# t1: " __loongarch_tune);
}

#pragma GCC push_options
#pragma GCC target("tune=la664")

void
t2 (void)
{
  asm volatile ("# t2: " __loongarch_tune);
}

#pragma GCC pop_options

void
t3 (void)
{
  asm volatile ("# t3: " __loongarch_tune);
}
