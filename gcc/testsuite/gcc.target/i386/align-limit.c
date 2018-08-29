/* { dg-do compile } */
/* { dg-options "-O2 -falign-functions=64 -flimit-function-alignment -march=amdfam10 -fcf-protection=none" } */
/* { dg-final { scan-assembler ".p2align 6,,1" } } */
/* { dg-final { scan-assembler-not ".p2align 6,,63" } } */

void
test_func (void)
{
}
