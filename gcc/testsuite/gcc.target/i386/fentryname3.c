/* { dg-do compile } */
/* { dg-require-effective-target mfentry } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-pg -mfentry"  } */
/* { dg-final { scan-assembler "section.*__entry_loc" } } */
/* { dg-final { scan-assembler "0x0f, 0x1f, 0x44, 0x00, 0x00" } } */
/* { dg-final { scan-assembler-not "__fentry__" } } */

__attribute__((fentry_name("nop"), fentry_section("__entry_loc")))
void foo(void)
{
}
