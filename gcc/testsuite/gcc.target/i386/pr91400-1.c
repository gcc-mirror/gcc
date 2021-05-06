/* PR target/91400 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "andl" 1 } } */
/* { dg-final { scan-assembler-times "cmpl" 1 } } */
/* { dg-final { scan-assembler-times "sete" 1 } } */
/* { dg-final { scan-assembler-not "cmove" } } */

_Bool
f (void)
{
  return __builtin_cpu_supports("popcnt")
	 && __builtin_cpu_supports("ssse3");
}
