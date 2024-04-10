/* { dg-do compile } */

extern int var;

void
func (void)
{
  __asm__ ("@ %p0" : : "Ws" (func));
  __asm__ ("@ %p0" : : "Ws" (&var + 1));
}

/* { dg-final { scan-assembler "@ _?func" } } */
/* { dg-final { scan-assembler "@ (_?var\\+4|4\\+_?var)" } } */
