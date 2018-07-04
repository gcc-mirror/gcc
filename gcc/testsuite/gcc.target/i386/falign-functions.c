/* { dg-do compile } */
/* { dg-options "-O2 -falign-functions=64:8" } */
/* { dg-final { scan-assembler ".p2align 6,,7" } } */

void
test_func (void)
{
}
