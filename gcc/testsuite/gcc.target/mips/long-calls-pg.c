/* { dg-do compile } */
/* { dg-mips-options "-O2 -mabi=32 -march=mips32 -fno-pic -pg -mno-abicalls -mlong-calls" } */
/* { dg-final { scan-assembler-not "\tjal\t_mcount" } } */
void
foo (void)
{
}
