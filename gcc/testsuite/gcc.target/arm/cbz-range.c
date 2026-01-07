/* { dg-do assemble } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-O -mthumb" } */
/* { dg-add-options arm_arch_v7a } */

#define f "movw r0, #0;movw r0, #0;movw r0, #0;"
#define f2 f f
#define f4 f2 f2
#define f8 f4 f4
#define f16 f8 f8
#define f32 f16 f16
#define f64 f32 f32
#define f128 f64 f64
#define f256 f128 f128
#define f512 f256 f256
#define f1024 f512 f512
#define f2048 f1024 f1024
#define f4096 f2048 f2048
#define f8192 f4096 f4096
#define f16384 f8192 f8192
#define f32768 f16384 f16384
#define f65536 f32768 f32768
#define f131072 f65536 f65536
int a;

int cbz1(int g)
{
  if (g)
    asm(f8);
  return a;
}

int cbz2(int g)
{
  asm ("": "+h"(g));
  if (g)
    asm(f8);
  return a;
}

int cbz3(int g)
{
  if (g)
    asm(f16);
  return a;
}

int cbz4(int g)
{
  asm ("": "+h"(g));
  if (g)
    asm(f16);
  return a;
}

int cbz5(int g)
{
  if (g)
    asm(f131072);
  return a;
}

int cbz6(int g)
{
  asm ("": "+h"(g));
  if (g)
    asm(f131072);
  return a;
}

int cbnz1(int g)
{
  if (!g)
    asm(f8);
  return a;
}

int cbnz2(int g)
{
  asm ("": "+h"(g));
  if (!g)
    asm(f8);
  return a;
}

int cbnz3(int g)
{
  if (!g)
    asm(f16);
  return a;
}

int cbnz4(int g)
{
  asm ("": "+h"(g));
  if (!g)
    asm(f16);
  return a;
}

int cbnz5(int g)
{
  if (!g)
    asm(f131072);
  return a;
}

int cbnz6(int g)
{
  asm ("": "+h"(g));
  if (!g)
    asm(f131072);
  return a;
}
