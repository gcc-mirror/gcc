/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef char  vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int   vec32 __attribute__((vector_size(8)));

extern vec8 foo1_8(void);
extern vec8 foo2_8(void);

vec8 fun8(void)
{
  return ~(foo1_8 () & foo2_8 ());
}

extern vec16 foo1_16(void);
extern vec16 foo2_16(void);

vec16 fun16(void)
{
  return ~(foo1_16 () & foo2_16 ());
}

extern vec32 foo1_32(void);
extern vec32 foo2_32(void);

vec32 fun32(void)
{
  return ~(foo1_32 () & foo2_32 ());
}


/* DeMorgan's Law's at work.  */
vec8 fun8b(void)
{
  return ~foo1_8 () | ~foo2_8 ();
}

vec16 fun16b(void)
{
  return ~foo1_16 () | ~foo2_16 ();
}

vec32 fun32b(void)
{
  return ~foo1_32 () | ~foo2_32 ();
}

/* { dg-final { scan-assembler-times "fnand\t%" 6 } } */
