/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw" } */

typedef char V1 __attribute__((vector_size (16)));
typedef short V2 __attribute__((vector_size (16)));
typedef char V5 __attribute__((vector_size (32)));
typedef short V6 __attribute__((vector_size (32)));
typedef int V7 __attribute__((vector_size (32)));

void
f1 (V1 *x)
{
  register V1 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V1) { 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpbroadcastb\[^\n\r]*xmm16" } } */

void
f2 (V2 *x)
{
  register V2 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V2) { 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpbroadcastw\[^\n\r]*xmm16" } } */

void
f5 (V5 *x)
{
  register V5 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V5) { 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpbroadcastb\[^\n\r]*ymm16" } } */

void
f6 (V6 *x)
{
  register V6 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V6) { 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler-not "vpbroadcastw\[^\n\r]*ymm16" } } */

void
f7 (V7 *x)
{
  register V7 a __asm ("xmm16");
  a = __builtin_shuffle (*x, (V7) { 0 });
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpbroadcastd\[^\n\r]*ymm16" } } */
