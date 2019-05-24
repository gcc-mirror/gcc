/* { dg-do compile } */
/* { dg-options "-O2 -falign-functions=64:8" } */
/* { dg-skip-if "SUBALIGN_LOG not set for Darwin" { *-*-darwin* } } */

void
a (void)
{
}

#pragma GCC push_options
#pragma GCC optimize "align-functions=128:100"
void b (void)
{
}
#pragma GCC pop_options

void
__attribute__((optimize("-falign-functions=88:88:32")))
c (void)
{
}

void
d (void)
{
}

/* { dg-final { scan-assembler-times ".p2align 6,,7" 2 } } */
/* { dg-final { scan-assembler-times ".p2align 7,,99" 1 } } */
/* { dg-final { scan-assembler-times ".p2align 7,,87" 1 } } */
/* { dg-final { scan-assembler-times ".p2align 5" 1 } } */
