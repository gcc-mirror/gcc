/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef char vec8 __attribute__((vector_size(4)));
typedef short vec16 __attribute__((vector_size(4)));

extern vec8 foo1_8(void);

vec8 fun8(void)
{
  return ~foo1_8 ();
}

extern vec16 foo1_16(void);

vec16 fun16(void)
{
  return ~foo1_16 ();
}

/* { dg-final { scan-assembler-times "fnot1s\t%" 2 } } */
