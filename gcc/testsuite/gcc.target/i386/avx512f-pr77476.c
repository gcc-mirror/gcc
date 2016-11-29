/* PR target/77476 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#ifndef PR77476_TEST
#include "avx512f-check.h"
#define PR77476_TEST avx512f_test
#endif

unsigned short s;
unsigned int i;
unsigned long long l;

void
f1 (void)
{
  unsigned char a = 0xff;
  asm volatile ("" : "+Yk" (a));
  s = a;
}

void
f2 (void)
{
  unsigned char a = 0xff;
  asm volatile ("" : "+Yk" (a));
  i = a;
}

void
f3 (void)
{
  unsigned char a = 0xff;
  asm volatile ("" : "+Yk" (a));
  l = a;
}

void
f4 (void)
{
  unsigned short a = 0xffff;
  asm volatile ("" : "+Yk" (a));
  i = a;
}

void
f5 (void)
{
  unsigned short a = 0xffff;
  asm volatile ("" : "+Yk" (a));
  l = a;
}

#ifdef __AVX512BW__
void
f6 (void)
{
  unsigned int a = 0xffffffff;
  asm volatile ("" : "+Yk" (a));
  l = a;
}
#endif

static void
PR77476_TEST ()
{
  f1 (); if (s != 0xff) __builtin_abort (); s = 0;
  f2 (); if (i != 0xff) __builtin_abort (); i = 0;
  f3 (); if (l != 0xff) __builtin_abort (); l = 0;
  f4 (); if (i != 0xffff) __builtin_abort (); i = 0;
  f5 (); if (l != 0xffff) __builtin_abort (); l = 0;
#ifdef __AVX512BW__
  f6 (); if (l != 0xffffffff) __builtin_abort (); l = 0;
#endif
}
