/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef int vec32 __attribute__((vector_size(4)));

extern vec32 foo1(void);
extern vec32 foo2(void);

vec32 bar(void)
{
  return foo1 () - foo2 ();
}

/* { dg-final { scan-assembler "fpsub32s\t%" } } */
