/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef char  vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int   vec32 __attribute__((vector_size(8)));

extern vec8 foo1_8(void);
extern vec8 foo2_8(void);

vec8 fun8(void)
{
  return foo1_8 () | foo2_8 ();
}

#ifndef __LP64__
/* Test the 32-bit splitter. */
vec8 fun8_2(vec8 a, vec8 b)
{
  return a | b;
}
#endif

extern vec16 foo1_16(void);
extern vec16 foo2_16(void);

vec16 fun16(void)
{
  return foo1_16 () | foo2_16 ();
}

#ifndef __LP64__
/* Test the 32-bit splitter. */
vec16 fun16_2(vec16 a, vec16 b)
{
  return a | b;
}
#endif

extern vec32 foo1_32(void);
extern vec32 foo2_32(void);

vec32 fun32(void)
{
  return foo1_32 () | foo2_32 ();
}

#ifndef __LP64__
/* Test the 32-bit splitter. */
vec32 fun32_2(vec32 a, vec32 b)
{
  return a | b;
}
#endif

/* { dg-final { scan-assembler-times "for\t%" 3 } } */
