/* { dg-do compile } */
/* { dg-options "-O2 -mabi=32 -pg -mno-abicalls -mlong-calls" } */
/* { dg-final { scan-assembler-not "\tjal\t_mcount" } } */
NOMIPS16 void
foo (void)
{
}
