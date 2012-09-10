/* { dg-options "-march=5kc" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tmul\t" } } */
/* { dg-final { scan-assembler-not "\tmadd\t" } } */
/* { dg-final { scan-assembler-not "\tmtlo\t" } } */
/* { dg-final { scan-assembler-not "\tmflo\t" } } */

NOMIPS16 int
f2 (int x, int y, int z)
{
  asm volatile ("" ::: "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9",
		"$10", "$11", "$12", "$13", "$14", "$15", "$16", "$17",
		"$18", "$19", "$20", "$21", "$22", "$23", "$24", "$25",
		"$31");
  return x * y + z;
}
