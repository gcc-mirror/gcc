/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef char  vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int   vec32 __attribute__((vector_size(8)));

extern vec8 foo1_8(void);
extern void foo2_8(vec8);

vec8 fun8(void)
{
  return ~foo1_8 ();
}

#ifndef __LP64__
/* Test the 32-bit splitter. */
vec8 fun8_2(vec8 a)
{
  foo2_8 (~a);
}
#endif

extern vec16 foo1_16(void);
extern void foo2_16(vec8);


vec16 fun16(void)
{
  return ~foo1_16 ();
}

#ifndef __LP64__
/* Test the 32-bit splitter. */
vec16 fun16_2(vec16 a)
{
  foo2_16 (~a);
}
#endif

extern vec32 foo1_32(void);
extern void foo2_32(vec8);

vec32 fun32(void)
{
  return ~foo1_32 ();
}

#ifndef __LP64__
/* Test the 32-bit splitter. */
vec32 fun32_2(vec32 a)
{
  foo2_32 (~a);
}
#endif

/* { dg-final { scan-assembler-times "fnot1\t%" 3 } } */
