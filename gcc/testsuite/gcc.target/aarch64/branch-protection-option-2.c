/* { dg-do "compile" } */
/* { dg-options "-mbranch-protection=pac-ret+leaf -mbranch-protection=none" } */

void foo2 ()
{
}

/* { dg-final { scan-assembler-not "\tautiasp\t" } } */
/* { dg-final { scan-assembler-not "\tpaciasp\t" } } */
