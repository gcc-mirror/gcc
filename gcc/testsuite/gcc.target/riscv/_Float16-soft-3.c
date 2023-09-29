/* { dg-do compile } */
/* { dg-options "-march=rv64if -mabi=lp64f -O" } */

int test_soft_compare (_Float16 a, _Float16 b)
{
    /* Make sure __gthf2 not invoked here. */
    /* { dg-final { scan-assembler-times "call\t__extendhfsf2" 2 } } */
    return a > b;
    /* { dg-final { scan-assembler-not "call\t__gthf2" } } */
    /* { dg-final { scan-assembler-times {\mfgt\.s\M} 1 } } */
}

