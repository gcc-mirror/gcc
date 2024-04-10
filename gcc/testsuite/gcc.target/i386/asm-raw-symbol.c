/* { dg-do compile } */

extern int var;

void
func (void)
{
  __asm__ ("@ %p0" : : "Ws" (func));
  __asm__ ("@ %p0" : : "Ws" (&var + 1));
}

/* { dg-final { scan-assembler "@ func" } } */
/* { dg-final { scan-assembler "@ var\\+4" } } */
